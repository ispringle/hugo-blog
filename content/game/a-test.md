+++
title = "A Test"
author = ["Ian S. Pringle"]
date = 2022-09-30T00:00:00-05:00
draft = false
+++

This is a test of embedding html and javascript into a markdown post. The idea
is that I can create little "doodles" on a canvas or something and then embed
those into a markdown file which will produce a post that is a mix of written
content and interactive content.

{{< rawhtml >}}
<script>
console.log("Hi")
var shapes = [];
var nestedShapes = [-100];

function animate() {
	var canvas = document.getElementById('bg');
	canvas.width = document.body.clientWidth;
	canvas.height = document.body.clientHeight;
	//drawGrid();
	var cw = canvas.width
	var ch = canvas.height
	var s = 200;
	for (var x=0;x<=cw+s;x+=s) {
		for (var y=0;y<=cw+s;y+=s) {
			shapes.push([x,y]);
		}
	}
	var t = 50;
	var max = 600
	for (var x=0;x<=max;x+=t) {
		nestedShapes.push(x);
	}


	//setInterval(drawShape, 100);
	//setInterval(drawNestedShape, 100);
	//setInterval(run, 100);
	window.requestAnimationFrame(loop);
}
function loop(timestamp) {
	drawNestedShape();
	window.requestAnimationFrame(loop);
}


function drawShape() {
	var canvas = document.getElementById('bg');
	if (canvas.getContext) {
		shapes.forEach(shape => {
			var x = shape[0];
			var y = shape[1];
			var ctx = canvas.getContext('2d');
			ctx.fillStyle = 'rgba(0,0,0,0.4)';
			ctx.strokeStyle = 'rgba(0,153,255,0.4)';
			ctx.save();
			ctx.translate(x, y);

			var time = new Date();
			ctx.rotate(((2*Math.PI)/6)*time.getSeconds() + ((2*Math.PI)/6000)*time.getMilliseconds());
			ctx.translate(0,0);
			ctx.beginPath();
			ctx.arc(95,50,40,0,2*Math.PI);
			ctx.stroke();
			ctx.restore();
		});
	}
}
function drawNestedShape() {
	var canvas = document.getElementById('bg');
	if (canvas.getContext) {
		nestedShapes.forEach(shape => {
			console.log(shape);
			var x = canvas.width/2;
			var y = canvas.height/2;
			var ctx = canvas.getContext('2d');
			ctx.fillStyle = 'rgba(0,0,0,0.4)';
			ctx.strokeStyle = 'rgba(0,153,255,0.4)';
			ctx.save();
			ctx.translate(x, y);

			var time = new Date();
			ctx.rotate(((2*Math.PI)/6)*time.getSeconds() + ((2*Math.PI)/6000)*time.getMilliseconds());
			ctx.translate(shape,0);
			ctx.beginPath();
			ctx.arc(95,50,40,0,2*Math.PI);
			ctx.stroke();
			ctx.restore();
		});
	}
}
function drawGrid() {
	var canvas = document.getElementById('bg');
	var cw = canvas.width
	var ch = canvas.height
	ctx = canvas.getContext('2d');

	for (var x=0;x<=cw;x+=50) {
		ctx.moveTo(x,2);
		ctx.lineTo(x,ch);
	}
	for (var y=0;y<=cw;y+=50) {
		ctx.moveTo(2,y);
		ctx.lineTo(cw,y);
	}
	ctx.strokeStyle = "black";
	ctx.stroke();
}

function run() {
	var canvas = document.getElementById('bg');
	if (canvas.getContext) {
		var ctx = canvas.getContext('2d');
		ctx.fillStyle = 'rgba(0,0,0,0.4)';
		ctx.strokeStyle = 'rgba(0,153,255,0.4)';
		ctx.save();
		ctx.translate(
			canvas.width/2,
			canvas.height/2);

		var time = new Date();
		ctx.rotate(((2*Math.PI)/6)*time.getSeconds() + ((2*Math.PI)/6000)*time.getMilliseconds());
		ctx.translate(400,1);
		ctx.beginPath();
		ctx.arc(95,50,40,0,2*Math.PI);
		ctx.stroke();
		ctx.restore();
	}
}
</script>
<div onload="animate();" style="overflow:hidden;width:100%;height:100%;margin:0;" >
<canvas id="bg"></canvas>
</div>
{{< /rawhtml >}}