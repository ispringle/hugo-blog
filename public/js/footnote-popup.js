function getFootnoteContent(index) {
  return document.getElementById("fn:" + index).cloneNode(true);
}

function removeRef(el) {
  const paragraphs = el.getElementsByTagName("p");
  const cleaned = [...paragraphs].flatMap((p) =>
    [...p.childNodes].filter((c) => c.className !== "footnote-backref")
  );
  return cleaned;
}

function footnotePopup() {
  const popupWrapper = document.querySelector("#popup-wrapper");

  popupWrapper.innerHTML = "";
  const popupContent = popupWrapper.appendChild(document.createElement("div"));
  popupContent.id = "popup-content";

  let popupIndex = null;
  popupIndex = popupWrapper.insertBefore(
    document.createElement("div"),
    popupContent
  );
  popupIndex.id = "popup-index";

  const fnRefs = document.querySelectorAll("sup[id^='fnref:']");
  fnRefs.forEach(function (el) {
    el.addEventListener("mouseenter", showRef(el));
    el.addEventListener("touchstart", showRef(el));

    el.addEventListener("click", closeRef);
    el.addEventListener("mouseleave", closeRef);
    el.addEventListener("touchend", closeRef);
    el.addEventListener("touchmove", closeRef);
  });

  function showRef(node) {
    return function () {
      const index = node.id.substring(6);

      const footnoteEl = getFootnoteContent(index);
      removeRef(footnoteEl).forEach((c) => popupContent.appendChild(c));

      popupIndex.innerHTML = index + ".";
      popupWrapper.style.display = "flex";
    };
  }

  function closeRef() {
    popupWrapper.style.display = "none";
  }
}

function footnotesToSide() {
  const fnClassName = "sidenote";

  const oldFootnotes = [...document.getElementsByClassName(fnClassName)];
  oldFootnotes.forEach((el) => el.parentNode.removeChild(el));

  const fnRefs = document.querySelectorAll("sup[id^='fnref:']");
  fnRefs.forEach(function (el) {
    const index = el.id.substring(6);

    const footnote = document.createElement("small");
    footnote.className = fnClassName;
    footnote.innerHTML = index + ".";

    removeRef(getFootnoteContent(index)).forEach((c) =>
      footnote.appendChild(c)
    );

    el.parentNode.insertBefore(footnote, el.nextSibling);
  });
}

function footnoteInit() {
  const footnoteSection = document.getElementsByClassName("footnotes")[0];
  if (!footnoteSection) return;
  const defaultFootnoteDisplay = footnoteSection.style.display;

  function handler(event) {
    if (event.matches) {
      footnoteSection.style.display = "none";
      footnotesToSide();
    } else {
      footnoteSection.style.display = defaultFootnoteDisplay;
      footnotePopup();
    }
  }
  const footnoteMediaQuery = window.matchMedia("(min-width: 1400px)");
  footnoteMediaQuery.addListener(handler);
  handler(footnoteMediaQuery);
}

footnoteInit();
