
global dir  "C:\Users\jgw12\Dropbox\Research\KGF"
cd "$dir"
		set more off
		set scheme lean2
		use GWF, clear
		replace gwf_leadername = "Sallal" if cow==678 & year==1966

		tsset gwf_caseid year
		gen newparty =support==1 & l.support==0
		gen yr = year if newparty==1
		egen yrs = max(yr), by(gwf_leaderid)
		tsset gwf_caseid year
		replace newparty=1 if l.newparty==1 & l.gwf_leaderid==gwf_leaderid & year==year[_n-1]+1
		gen createparty =militparty_new==1 | (newparty==1  & partyhistory_post==1) 

		* rename variable for plots *
		rename ldr_group_priordem ldrPriorD
		rename ldr_group_domparty ldrParty
		rename ldr_group_military ldrMil
		rename ldr_group_insurgency ldrRebel
		rename ldr_group_civsucc ldrCiv
		rename ldr_group_other ldrOther
		rename ldr_group_foreign ldrForgn
		rename ldr_group_hereditary ldrHered
		rename militparty_allyparty MilPartyAlly
		rename militparty_noparty  MilPartyNo
		rename militparty_priorparty MilPartyPrior
		rename militparty_newparty MilPartyNew
		rename electldr_notelect ElecldrNot
		rename electldr_priordict ElecldrPrDict
		rename electldr_priordem ElecldrPrDem
		rename electldr_1candidate Elecldr1C
		rename electldr_1faction  Elecldr1F
		rename electldr_multileg  ElecldrMLeg
		rename electldr_multiexec  ElecldrMExec
		rename ldr_exp_highrank LdrexHighR
		rename ldr_exp_lowrank LdrexLowR
		rename ldr_exp_rebel LdrexRebel
		rename ldr_exp_demelect LdrexDemEl
		rename ldr_exp_supportparty LdrexParty
		rename ldr_exp_pers_loyal LdrexLoyal
		rename ldr_exp_pers_relative LdrexReltv
		rename ldr_exp_rulingfamily LdrexRulFam
		rename ldr_exp_other LdrexOther
		rename paramil_pers ParamilPers
		rename paramil_party ParamilParty
		rename paramil_fightrebel ParamilFReb
		rename seizure_coup SeizCoup
		rename seizure_rebel SeizRebel
		rename seizure_uprising SeizUpris
		rename seizure_election SeizElec
		rename seizure_succession SeizSucc
		rename seizure_family SeizFam
		rename partyhistory_noparty PartyhNoparty
		rename partyhistory_postseizure PartyhPost
		rename partyhistory_priorelection PartyhElec
		rename partyhistory_priornosupport PartyhNoWin
		rename partyhistory_priorwonsupport PartyhWin
		rename partyhistory_insurgent PartyhReb
		rename partyhistory_priordem PartyhPriorDem
		
		
sort cow year 
 
gen caseid= gwf_caseid
gen ldrid = gwf_leaderid
egen ctag = tag(caseid)
egen spell = max(gwf_case_duration), by(caseid)
egen min =min(year), by(ldrid)
egen min1 = min(year), by(caseid)
gen xfirst = ldrid if year == min1
gen f =ldrid==xfirst
egen firstldr =max(f), by(ldrid)
drop min min1 xfirst f
egen min = min(gwf_leader_firstyear),by(caseid)
replace ldr1styear = min



* Figure 1 *  exclude monarchies
twoway (hist spell if ctag==1 & LdrexRulFam==0, bin(50) freq xtitle(Years in power) text(15 63 "North Korea", place(e)))/*
*/ (hist spell if ctag==1 & cow==731, bin(50) color(red) legend(off)) (pcarrowi 14 69 1 63,scheme(lean2)/*
*/ ylab(,glcolor(gs16)) title(Autocratic regime duration) ytitle(Number of regimes) )
graph export "$dir/Regime-Durations.pdf", as(pdf) replace

* Inherit and Divided *
gen inheritparty = (PartyhWin==1|PartyhNoWin==1| PartyhReb==1 | PartyhPriorDem==1) if gwf_case_duration==1 | year==1946
egen inh= max(inheritparty),by(caseid)   /* ensure no within case variation */
replace inherit = inh
gen divided = SeizUpr==1 | (SeizCoup==1 & LdrexLowR==1) | (SeizCoup==1 & foreign==1) if gwf_case_duration==1 | year==1946
egen div = max(divided), by(caseid)      /* ensure no within case variation */
replace divided =div
drop div inh
tab inherit divided if ctag==1, col
tab inherit divided if ctag==1, row
			
* Personalism index *
recode milmerit_pers (2=1)
recode milnotrial (1=0) (0=1)
recode pleb (1=2) if support==1 & pleb==1
gen reloffice = officepers + leaderrel
gen LdrPers = LdrexReltv==1 | LdrexLoyal==1
gen purge = milnotrial
recode purge (0=1) (1=0)

global pvars = "LdrPers reloffice pleb create partyrbrs partyexcom_pers ParamilPe sectyapp_pers milmerit_pe purge"
pca $pvars, mineigen(1)
screeplot
predict orthog_pers, center
rotate, promax  factors(1)
predict xpers, center  /* oblique */
egen z2xpers = std(xpers)
replace xpers =  (z2xpers)
egen tag  = tag(year)
egen yrm=mean(xpers),by(year)
alpha $pvars, gen(zpers) std
corr zpers xpers


twoway (line xpers year if cow==710 & year>1948 & year<2011, sort lcolor(red)  lpattern(solid)) /*
*/ (line  xpers year if cow==731 & year>1948 & year<2011, sort lcolor(blue) lpattern(solid) ylab(-1.5 (.5) 1.5) /*
*/ xscale(range (1950 2010)) xlabel(1950 (10) 2010) title("Personalism in 3 Communist regimes") /*
*/ ytitle("Personalism score") xtitle("Year",height(6))  legend(lab(1 "China") lab(2 "North Korea")  /*
*/ lab(3 "Vietnam") lab(4 "Average") pos(6) col(4) ring(1))) (line  xpers year if cow==816 & year>1948 & year<2011, /*
*/ sort lpattern(solid) lcolor(green) scheme(lean2) ylab(,glcolor(gs16))) /*
*/ (line  yrm year if tag==1 & year>1948 & year<2011,sort  lpattern(solid) lcolor(gs12))
graph export "$dir/China-NKorea-Vietnam.pdf", as(pdf) replace

		
********************
* Individual items *
********************
  * plebiscite *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line pleb year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid) saving(h4, replace) title(Plebiscite) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
    * Create party *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line create year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid) saving(h1, replace) title(Create new party) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle("")) 
	* Rbr stamp *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line partyrbrstmp year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h3, replace) title(Rubber-stamp party) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
	* Party excom *
twoway (line  xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line partyexcom_pers year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h2, replace) title(Party exec committee) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
	* Paramilitary *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line ParamilPers year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h5, replace) title(Paramilitary) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
	* Military promotions * 
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line milmerit_pers year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h6, replace) title(Military promotions) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
	 * Security Apparatus *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line sectyapp_pers year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h7, replace) title(Security apparatus) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
	 * Military purges *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line purge year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h8, replace) title(Purges) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
	* Leader selected *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line LdrPers year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h9, replace) title(Leader selection) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
	* Office pers *
twoway (line xpers year if cow==731 & year>1948 & year<2011,lcolor(gs8)lpattern(dash)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line reloffice year if cow==731 & year>1948 & year<2011,lcolor(blue)lpattern(solid)  saving(h10, replace) title(High office) /*
*/ legend(off) scheme(lean2) ylab(,glcolor(gs16)) xtitle(""))
 graph combine h1.gph h2.gph h3.gph h4.gph h5.gph h6.gph h7.gph h8.gph h9.gph h10.gph, col(2) ysize(8) l1(Personalism index) b1(Year) iscale(.58)
graph export "$dir/Personalist-Items.pdf", as(pdf) replace


mca create pleb partyrbrstmp partyexcom_pers, dimensions(1)
predict p_pers, normal(pr) dimen(1)
replace p_pers = p_pers*-1
egen z2p_pers = std(p_pers)
replace p_pers =  (z2p_pers)

mca sectyapp_pers milmerit_pers ParamilPers milnotrial, dimensions(1)
predict m_pers, normal(pr) dimen(1)
replace m_pers = m_pers*-1
egen z2m_pers = std(m_pers)
replace m_pers =  (z2m_pers)

twoway (line p_pers year if cow==731 & year>1948 & year<2011,lcolor(red)xscale(range (1950 2010)) xlabel(1950 (10) 2010)) /*
*/ (line m_pers year if cow==731 & year>1948 & year<2011,lcolor(blue) title(Two dimensions of Personalism) /*
*/ legend(lab(1 "Party") lab(2 "Military") pos(6) col(2) ring(1)) scheme(lean2) ylab(,glcolor(gs16)) ytit(Scores) xtit(Year))
drop z2* 
graph export "$dir/Sequence.pdf", as(pdf) replace



	forval i=1(1)10 {
		erase h`i'.gph
	}

				   
