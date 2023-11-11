window.addEventListener("load", () => {
// Find SVG Root
let svg = document.children[0];

// Find origin
let originEl = document.getElementById("origin");
let { x, width, y, height } = originEl.getBBox()
let getCenterOfBox = ({ x, width, y, height }) => {
  let centerX = x + width / 2;
  let centerY = y + height / 2;
  return [centerX, centerY];
}
let [originX, originY] = getCenterOfBox(originEl.getBBox());

// Create a rotation transformation object to attach to all inner stator elements
let transformation = svg.createSVGTransform();
let dScaleEls = [...document.getElementsByClassName("dScale"), ...document.getElementsByClassName("lScale")];
let setRotation = angle => {
  transformation.setRotate(angle, originX, originY);
  for (let el of dScaleEls) {
    el.transform.baseVal.clear();
    el.transform.baseVal.appendItem(transformation);
  }
}

// Detect clicking
let relativeToOrigin = e => {
  let { clientX, clientY } = e;
  let [originX, originY] = getCenterOfBox(originEl.getBoundingClientRect());
  return 180 * Math.atan2(clientX - originX, clientY - originY) / Math.PI;
}

{
  let mouseIsDown = false;
  let startingAngle = 0;
  let totalAngle = 0;
  let differenceAngle = 0;
  document.addEventListener("mousedown", e => {
    mouseIsDown = true;
    scalingBy = e.buttons === 4 ? 0.1 : 1;
    startingAngle = relativeToOrigin(e);
  });

  document.addEventListener("mousemove", e => {
    if (mouseIsDown) {
      let endingAngle = relativeToOrigin(e);
      let delta = (720 + startingAngle - endingAngle) % 360;
      delta = scalingBy * (delta > 180 ? delta - 360 : delta);
      differenceAngle = (totalAngle + delta) % 360;
      console.log(differenceAngle, delta);
      setRotation(differenceAngle);
    }
  });

  document.addEventListener("mouseup", e => {
    mouseIsDown = false;
    totalAngle = differenceAngle;
  });
}
});
