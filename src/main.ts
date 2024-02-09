import './style.css'
import p5 from "p5"
import {RDouble, WebR} from "webr"


let start=false;

let selected=3;
let element=document.getElementById("app") as HTMLDivElement

let bMass=document.getElementById("bMass") as HTMLInputElement;
let rMass=document.getElementById("rMass") as HTMLInputElement;
let l1Length=document.getElementById("l1Length") as HTMLInputElement;
let l2Length=document.getElementById("l2Length") as HTMLInputElement;
let Ang1=document.getElementById("Ang1") as HTMLInputElement;
let Ang2=document.getElementById("Ang2") as HTMLInputElement;
let explicite=document.getElementById("explicite") as HTMLHeadingElement;
let ameliore=document.getElementById("ameliore") as HTMLHeadingElement;
let modifie=document.getElementById("modifie") as HTMLHeadingElement;
let rk4=document.getElementById("rk4") as HTMLHeadingElement;
let taylor=document.getElementById("taylor") as HTMLHeadingElement;
let st=document.getElementById("start") as HTMLHeadingElement;
let a1=Ang1.valueAsNumber;
let a2=Ang2.valueAsNumber;
const red ="#b0453b"
const blue ="#5f95ca"
const black = "#4c4c4c"
let methods=[explicite,ameliore,modifie,rk4]
const webR = new WebR();
await webR.init();
let rFunction=await fetch("ch7.r");
webR.evalR(await rFunction.text())


async function calculate(delta:number){
  let k;
  switch (selected) {
    case 0:
        k=await webR.evalR(`get_cords_euler(${delta})`) as RDouble;
      break;
    case 1:
        k=await webR.evalR(`get_cords_eulerM(${delta})`) as RDouble;
      break;
    case 2:
      k=await webR.evalR(`get_cords_eulerA(${delta})`) as RDouble;
      break;
    case 3:
      k=await webR.evalR(`get_cords_rk4(${delta})`) as RDouble;
      break;
    case 4:
      k=await webR.evalR(`get_cords_taylor(${delta})`) as RDouble;
      break;
    default:
      break;
  }
  return k;
}

explicite.addEventListener("click",async ()=>{
  if (selected !==0&&!start){
    methods[selected].classList.remove("selected")
    selected=0;
    explicite.classList.add("selected")
    await webR.evalR("x<-c(0,0,x[3],x[4])")
  }
})

ameliore.addEventListener("click",async ()=>{
  if (selected !==1&&!start){
    methods[selected].classList.remove("selected")
    selected=1;
    ameliore.classList.add("selected")
    await webR.evalR("x<-c(0,0,x[3],x[4])")
  }
})

modifie.addEventListener("click",async ()=>{
  if (selected !==2&&!start){
    methods[selected].classList.remove("selected")
    selected=2;
    modifie.classList.add("selected")
    await webR.evalR("x<-c(0,0,x[3],x[4])")
  }
})

rk4.addEventListener("click",async ()=>{
  if (selected !==3 &&!start){
    methods[selected].classList.remove("selected")
    selected=3;
    rk4.classList.add("selected")
    await webR.evalR("x<-c(0,0,x[3],x[4])")
  }
})
taylor.addEventListener("click",async ()=>{
  if (selected !==4 &&!start){
    methods[selected].classList.remove("selected")
    selected=4;
    taylor.classList.add("selected")
    await webR.evalR("x<-c(0,0,x[3],x[4])")
  }
})

//////////////////////////////////////////////////////////////////


bMass.addEventListener("change",()=>{
  webR.evalR(`m1<-${bMass.valueAsNumber};x<-c(0,0,x[3],x[4])`)
})

rMass.addEventListener("change",()=>{
  webR.evalR(`m2<-${rMass.valueAsNumber};x<-c(0,0,x[3],x[4])`)
})

l1Length.addEventListener("change",()=>{
  webR.evalR(`l1<-${l1Length.valueAsNumber};x<-c(0,0,x[3],x[4])`)
})

l2Length.addEventListener("change",()=>{
  webR.evalR(`l2<-${l2Length.valueAsNumber};x<-c(0,0,x[3],x[4])`)
})

Ang1.addEventListener("change",()=>{
  webR.evalR(`a1<-${Ang1.valueAsNumber}`)
  a1=Ang1.valueAsNumber
})

Ang2.addEventListener("change",()=>{
  webR.evalR(`a2<-${Ang2.valueAsNumber}`)
  a2=Ang2.valueAsNumber
})

st.addEventListener("click",()=>{
  if (start){
    start=false;
    st.innerText="Start";
    bMass.disabled=false;
    rMass.disabled=false;
    l1Length.disabled=false;
    l2Length.disabled=false;
    Ang1.disabled=false;
    Ang2.disabled=false;
  }else{
    webR.evalR(`a1<-${a1}`)
    webR.evalR(`a2<-${a2}`)
    webR.evalR("x<-c(x[1],x[2],a1,a2)")
    start=true;
    st.innerText="Stop"
    bMass.disabled=true;
    rMass.disabled=true;
    l1Length.disabled=true;
    l2Length.disabled=true;
    Ang1.disabled=true;
    Ang2.disabled=true;
  }
})




new p5((p:p5) => {
  let px0=p.width/2;

  let py0=200;
  let pt1=p.PI;
  let pt2=0.05;
  let sc=100;

    p.setup = () => {
      
    let L1=l1Length.valueAsNumber;
    let L2=l2Length.valueAsNumber;
    p.createCanvas(element.clientWidth,element.clientHeight).parent(element)
    px0=p.width/2;
    // p.fill(200)
    // p.stroke(200)
    
    let x1=(L1*p.sin(pt1))*sc +px0;
    let y1=L1*p.cos(pt1)*sc +py0;
    let x2=x1+ L2*p.sin(pt2)*sc;
    let y2=y1 +L2*p.cos(pt2)*sc;    

    p.stroke(black)
    p.line(px0,py0,x1,y1)
    p.line(x1,y1,x2,y2)
    p.fill(black)
    p.circle(px0,py0,20)
    p.fill(blue)
    p.circle(x1,y1,40)
    p.fill(red)
    p.circle(x2,y2,40) 
  }

 p.draw = async () => {
  let L1=l1Length.valueAsNumber;
  let L2=l2Length.valueAsNumber;

    if (start){
      let cords=await calculate(p.deltaTime*1.2/1000)
      let [pt1,pt2]=await (cords as RDouble).toArray() as number[];

      a1=pt1;
      a2=pt2;
      if (Math.abs(Ang1.valueAsNumber-pt1)>0.2){        
        Ang1.valueAsNumber=pt1;
        Ang2.valueAsNumber=pt2;
      }
      p.background("#c8c8c8")
      
      let x1=(L1*p.sin(pt1))*sc +px0;
      let y1=L1*p.cos(pt1)*sc +py0;
      let x2=x1+ L2*p.sin(pt2)*sc;
      let y2=y1 +L2*p.cos(pt2)*sc;    
  
  
      p.stroke(black)
      p.line(px0,py0,x1,y1)
      p.line(x1,y1,x2,y2)
      p.fill(black)
      p.circle(px0,py0,20)
      p.fill(blue)
      p.circle(x1,y1,40)
      p.fill(red)
      p.circle(x2,y2,40) 
    }else{

      p.background("#c8c8c8")

      let x1=(L1*p.sin(a1))*sc +px0;
      let y1=L1*p.cos(a1)*sc +py0;
      let x2=x1+ L2*p.sin(a2)*sc;
      let y2=y1 +L2*p.cos(a2)*sc;    
  
  
      p.stroke(black)
      p.line(px0,py0,x1,y1)
      p.line(x1,y1,x2,y2)
      p.fill(black)
      p.circle(px0,py0,20)
      p.fill(blue)
      p.circle(x1,y1,40)
      p.fill(red)
      p.circle(x2,y2,40) 
    }

  }
})