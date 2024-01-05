import './style.css'
import p5 from "p5"
import {RDouble, RFunction, WebR} from "webr"

let element=document.getElementById("app") as HTMLDivElement


const webR = new WebR();


await webR.init();


let rFunction=await fetch("ch7.r");

webR.evalR(await rFunction.text())

let  get_cords=await webR.evalR('get_cords') as RFunction;


console.log('hello');

// console.log(await get_cords());


new p5((p:p5) => {
  let px0=p.width/2;
  let L1=1.5;
  let L2=1;
  let py0=200;
  let pt1=p.PI;
  let pt2=0.05;
  let sc=100;

    p.setup = () => {
      
    p.createCanvas(element.clientWidth,element.clientHeight).parent(element)
    px0=p.width/2;
    p.fill(200)
    p.stroke(200)
    
    let x1=(L1*p.sin(pt1))*sc +px0;
    let y1=L1*p.cos(pt1)*sc +py0;
    let x2=x1+ L2*p.sin(pt2)*sc;
    let y2=y1 +L2*p.cos(pt2)*sc;    

    p.circle(px0,py0,20)
    p.line(px0,py0,x1,y1)
    p.circle(x1,y1,40)
    p.line(x1,y1,x2,y2)
    p.circle(x2,y2,40) 
  }
//   x1 <- L1*sin(x[3])
// y1 <- -L1*cos(x[3])
// x2 <- x1 + L2*sin(x[4])
// y2 <- y1 - L2*cos(x[4])
  p.draw = async () => {
    p.fill(200)
    p.stroke(200)
    // let x=await get_cords()
    // console.log(p.deltaTime);
    
    let cords=await webR.evalR(`get_cords(${p.deltaTime*1.2/1000})`) as RDouble;
    // let cords=await get_cords(p.deltaTime/1000) as RDouble
    
    // console.log(co);
    
    let [pt1,pt2]=await cords.toArray() as number[]
    
    
    p.background(10,10,10)
    
    let x1=(L1*p.sin(pt1))*sc +px0;
    let y1=L1*p.cos(pt1)*sc +py0;
    let x2=x1+ L2*p.sin(pt2)*sc;
    let y2=y1 +L2*p.cos(pt2)*sc;    

    p.circle(px0,py0,20)
    p.line(px0,py0,x1,y1)
    p.circle(x1,y1,40)
    p.line(x1,y1,x2,y2)
    p.circle(x2,y2,40) 
  }
})