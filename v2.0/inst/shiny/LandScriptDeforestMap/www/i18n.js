/* Translate interface text in place, preserving Shiny bindings and input values. */
(function () {
  "use strict";
  const config = JSON.parse(document.getElementById("landscript-translations").textContent);
  const dictionary = config.text;
  const fragments = config.fragments.slice().sort((a, b) => b.length - a.length);
  const escapeRegex = text => text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const pattern = new RegExp(fragments.map(escapeRegex).join("|"), "gu");
  const originals = new WeakMap();
  const attributes = new WeakMap();
  let language = "pt-BR";
  let scheduled = false;

  function translate(text) {
    if (language !== "en") return text;
    const core = text.trim();
    if (Object.prototype.hasOwnProperty.call(dictionary, core)) {
      return text.replace(core, () => dictionary[core]);
    }
    return text.replace(/([0-9]+) de ([0-9]+)/g, "$1 of $2")
      .replace(pattern, match => dictionary[match]);
  }

  // Dataset cells, column names, dropdown values and editable content belong
  // to the user. They must never be translated as interface copy.
  const excluded = "script, style, textarea, input, select, option, pre, code, " +
    ".dataTables_wrapper table, .dt-container table, .selectize-dropdown, " +
    ".selectize-input .item, .sF-file-name, .sF-dirList, .sF-breadcrum, " +
    "[contenteditable='true'], [data-no-i18n]";

  function updateText(node) {
    if (!node.parentElement || node.parentElement.closest(excluded)) return;
    const current = node.nodeValue;
    let record = originals.get(node);
    // Shiny may reuse a text node for new output. Save that new source, too.
    if (!record || current !== record.rendered) record = { source: current };
    const rendered = translate(record.source);
    if (current !== rendered) node.nodeValue = rendered;
    record.rendered = rendered;
    originals.set(node, record);
  }

  function updateAttributes(element) {
    if (element.closest("[data-no-i18n], .dataTables_wrapper table, .dt-container table")) return;
    let records = attributes.get(element);
    if (!records) records = {};
    ["title", "placeholder", "aria-label", "alt", "data-title"].forEach(name => {
      if (!element.hasAttribute(name)) return;
      const current = element.getAttribute(name);
      let record = records[name];
      if (!record || current !== record.rendered) record = { source: current };
      const rendered = translate(record.source);
      if (current !== rendered) element.setAttribute(name, rendered);
      record.rendered = rendered;
      records[name] = record;
    });
    attributes.set(element, records);
  }

  // Translate only app-defined choice labels. Keep option values and dataset
  // columns, layer names and category values exactly as supplied in the file.
  const choiceLabels = {
    "analysis-mapbiomas": { none: "Sem legenda", custom: "Personalizar",
      "4": "MapBiomas - Coleção 4", "7.1": "MapBiomas - Coleção 7.1",
      "8": "MapBiomas - Coleção 8", "10": "MapBiomas - Coleção 10", "11": "MapBiomas - Coleção 11" },
    "analysis-result_level": { grid: "Grid_ID", mesh: "Malha", groups: "Grupos", groups_1: "Grupos 1", groups_2: "Grupos 2", total: "Total" },
    "visuals-auto_level": { grid: "Grid_ID", mesh: "Malha", groups: "Grupos", groups_1: "Grupos 1", groups_2: "Grupos 2", total: "Total" },
    "visuals-map_group": { "": "Nenhuma" },
    "visuals-highlight": { "": "Nenhuma" },
    "visuals-map_north_arrow": { tl: "Superior esquerdo", tr: "Superior direito" }
  };
  function choiceLabel(id, value, original) {
    if (choiceLabels[id]) return choiceLabels[id][value];
    if (id === "visuals-chart_group") {
      if (value === "__none__") return "Sem agrupamento";
      if (value.startsWith("__level__:")) {
        const level = value.slice("__level__:".length);
        const labels = Object.assign({}, choiceLabels["visuals-auto_level"], { mesh: "Malha (ID_mesh)" });
        if (labels[level]) return "Nível: " + labels[level];
      }
      if (value.startsWith("__column__:")) {
        // Only translate the prefix, even if a user column is named "Grupos".
        return { prefix: "Coluna: ", suffix: value.slice("__column__:".length) };
      }
    }
    if (id === "visuals-gpkg_layer" && value === "" && ["Tabela", "Table", "Camada única", "Single layer"].includes(original)) {
      return original === "Tabela" || original === "Table" ? "Tabela" : "Camada única";
    }
    return undefined;
  }
  function updateChoices() {
    document.querySelectorAll("select").forEach(select => {
      const widget = select.selectize;
      const options = widget ? Object.entries(widget.options) : Array.from(select.options, option => [option.value, { text: option.text }]);
      let changed = false;
      options.forEach(([value, option]) => {
        const source = choiceLabel(select.id, value, option.text);
        if (source === undefined) return;
        const label = typeof source === "string" ? translate(source) : translate(source.prefix) + source.suffix;
        if (label === option.text) return;
        if (widget) {
          // Mutate label metadata and invalidate the display cache, avoiding
          // updateOption(), which can trigger change events for selected items.
          option[widget.settings.labelField] = label;
          widget.clearCache("option");
          widget.clearCache("item");
          const item = widget.getItem(value);
          if (item.length) item.replaceWith(widget.render("item", option));
          changed = true;
        } else {
          Array.from(select.options).filter(o => o.value === value).forEach(o => { o.text = label; });
        }
      });
      if (widget && changed) {
        if (widget.isOpen) widget.refreshOptions(false);
      }
      if (widget && select.id === "analysis-group_column") {
        const placeholder = translate("Nenhuma coluna = toda a área");
        widget.settings.placeholder = placeholder;
        if (widget.$control_input.attr("placeholder") !== placeholder) widget.$control_input.attr("placeholder", placeholder);
      }
    });
  }

  function apply() {
    scheduled = false;
    if (!document.body) return;
    document.documentElement.lang = language;
    document.title = translate("LandScriptDeforestMap — Plataforma Shiny");
    const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
    let node;
    while ((node = walker.nextNode())) updateText(node);
    document.body.querySelectorAll("[title], [placeholder], [aria-label], [alt], [data-title]").forEach(updateAttributes);
    updateChoices();
  }
  function schedule() {
    if (!scheduled) { scheduled = true; window.requestAnimationFrame(apply); }
  }
  function setLanguage(value) { language = value === "en" ? "en" : "pt-BR"; apply(); }

  window.LandScriptI18n = { translate, setLanguage, apply };
  Shiny.addCustomMessageHandler("landscript-language", message => setLanguage(message.language));
  function start() {
    new MutationObserver(schedule).observe(document.body, {
      childList: true, characterData: true, subtree: true, attributes: true,
      attributeFilter: ["title", "placeholder", "aria-label", "alt", "data-title"]
    });
    apply();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
