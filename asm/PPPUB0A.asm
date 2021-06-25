*          DATA SET PPPUB0A    AT LEVEL 017 AS OF 01/03/05                      
*PHASE T4060AA                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T4060A PROGRAM UPDATE LOG'                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 12/10/04 SUBMEDIA, DAYS ADD TO INVOICE DATE (PPGENPUBPY)                 
*                                                                               
* SMYE 05/02    PUBVAL AND PUBEDIT CORE-RESIDENT                                
*                                                                               
* BPLA 03/98    WAS PPPUB0AP - RENAMED 3/10/98                                  
*               OLD PPPUB0A IS NOW PPPUB0AS (NEEDS OLD SCREEN)                  
*                                                                               
* BPLA 05/96    PAY CONTROLS FOR ALL AGENCIES                                   
*                                                                               
* BPLA 03/96    USE PUGENEROL AND PUGENOLD INSTEAD OF PPGENEROL                 
*                                                                               
* BPLA 11/95    EXPAND ELEMENT AND ADD NEW $PAY CONTROLS                        
*                                                                               
* BPLA 04/05/95 ADD PAY ONLY IF CASH RECEIVED FIELD                             
*               ONLY DISPLAY FOR SJR, HDTO AND DOREMUS FOR NOW                  
*                                                                               
* BPLA 10/02/91 UPDATE FOR REP NAME SEARCHING AND TO USE PPPUBWRK               
*                                                                               
* ROSA 11/09/88 CLEAR REP AND VENDOR DESCRIPTIONS                               
*                                                                               
* ROSA 10/04/88 ADD NEW FIELDS FOR PRINTING STANDARD COMMENTS 1+2               
*               TO INSERTION ORDER PGMS                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T4060A PUBFILE CLIENT REP OVERRIDE'                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T4060A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PPPUB0AX-PPPUB0AD,T4060A,RR=RE,CLEAR=YES                         
*                                                                               
         LR    R8,RC                                                            
         USING PPPUB0AD,R8         R8 = A(LOCAL STORAGE)                        
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    RE,RELO0A                                                        
*                                                                               
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
*                                                                               
         BRAS  RE,INITWKST         INITIALIZE WORKING STORAGE                   
*                                                                               
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BRAS  RE,GETPUB                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVC   KEY+27(4),PUBADDR                                                
         BRAS  RE,GETPUB                                                        
*                                                                               
         MVI   LTLIND,0                                                         
CKREPEL  LA    R7,PUBREC+33                                                     
         LA    R6,CKREP1                                                        
         LA    R2,CKIND                                                         
CKREP1   CLI   0(R7),X'14'                                                      
         BNE   NEXTEL                                                           
         CLC   BCLT(3),2(R7)                                                    
         BNE   NEXTEL                                                           
         OI    LTLIND,X'10'    REP ELEMENT EXISTS INDICATOR                     
*                                                                               
         CLI   BACT,B$ADD          ADD                                          
         BNE   SEEIFORM                                                         
         TM    LTLIND,X'30'       CANNOT HAVE AN ELEMENT EXITING W/             
         BZ    SEEIFORM           AN ADD FUNCTION                               
         LA    R3,COMBERR                                                       
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
*                                                                               
SEEIFORM CLI   BYTE2,B$FORM        SEE IF ACTION =FORMAT                        
         BE    FORMATP                                                          
*                                CLIENT SCREEN IN TWA SO EDIT IT                
*                                UNLESS ACTION= SRDS OR DISPLAY                 
         CLI   BACT,B$CHA                                                       
         BH    FORMATP                                                          
*                                                                               
         MVC   SPUBCVEN,17(R7)    SAVE VENDOR NUMBER FOR POSS DELETE            
         GOTO1 VRECUP,DMCB,(1,PUBREC),(R7)   DELETE THIS ELEMENT                
*                                 WILL BE ADDED IF EDIT OK                      
         B     CKIND                                                            
*                                                                               
NEXTEL   CLI   0(R7),0                                                          
         BE    0(R2)               END OF RECORD  RETURN VIA R2                 
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         BR    R6                  NEXT ELEMENT RETURN VIA R6                   
*                                                                               
CKIND    DS    0H                                                               
         CLI   BYTE2,B$FORM        SEE IF ACTION =FORMAT                        
         BE    FORMATP                                                          
*                                                                               
         CLI   BACT,B$CHA                                                       
         BH    FORMATP                                                          
         XC    ELEAREA(250),ELEAREA                                             
*                                                                               
         XC    CLIPRN,CLIPRN        CLEAR REP NAMES                             
         XC    CLITRN,CLITRN                                                    
         XC    CLICRN,CLICRN                                                    
         XC    CLIPUBN,CLIPUBN                                                  
         FOUT  CLITRNH                                                          
         FOUT  CLICRNH                                                          
         FOUT  CLIPUBNH                                                         
         FOUT  CLICOM1H                                                         
         FOUT  CLIWHR1H                                                         
         FOUT  CLICOM2H                                                         
         FOUT  CLIWHR1H                                                         
*                                                                               
* CLIENT SCREEN IN TWA SO EDIT IT, UNLESS ACTION IS DISPLAY                     
*                                                                               
EDIT     B     FINDREL                                                          
*                                                                               
         USING LTLREPD,R4                                                       
EDITR    XC    PUBPAREP,PUBPAREP                                                
         XC    CLIPRN,CLIPRN                                                    
         FOUT  CLIPRNH                                                          
         MVI   ESWITCH,0                                                        
         CLI   CLIPREPH+5,0                                                     
         BE    CKTREP                                                           
         LA    R2,CLIPREPH                                                      
         GOTOR EDTREP                                                           
         BNE   EXIT                ERROR OCCURED                                
         MVI   ESWITCH,1                                                        
         MVC   PUBPAREP,CLIPREP                                                 
         FOUT  CLIPREPH                                                         
         FOUT  CLIPRNH,PREPNAME,30                                              
*                                                                               
CKTREP   XC    PUBTRREP,PUBTRREP                                                
         XC    CLITRN,CLITRN                                                    
         FOUT  CLITRNH                                                          
         CLI   CLITREPH+5,0                                                     
         BE    CKCREP                                                           
         LA    R2,CLITREPH                                                      
         GOTOR EDTREP                                                           
         BNE   EXIT                ERROR OCCURED                                
         MVI   ESWITCH,1                                                        
         MVC   PUBTRREP,CLITREP                                                 
         FOUT  CLITREPH                                                         
         FOUT  CLITRNH,PREPNAME,30                                              
*                                                                               
CKCREP   XC    PUBCNREP,PUBCNREP                                                
         XC    CLICRN,CLICRN                                                    
         FOUT  CLICRNH                                                          
         CLI   CLICREPH+5,0                                                     
         BE    CKVEN                                                            
         LA    R2,CLICREPH                                                      
         GOTOR EDTREP                                                           
         BNE   EXIT                ERROR OCCURED                                
         MVI   ESWITCH,1                                                        
         MVC   PUBCNREP,CLICREP                                                 
         FOUT  CLICREPH                                                         
         FOUT  CLICRNH,PREPNAME,30                                              
         DROP  R4                                                               
*                                                                               
CKVEN    DS    0H                                                               
         XC    CLIPUBN,CLIPUBN                                                  
         FOUT  CLIPUBNH                                                         
         LA    R2,CLIDESH                                                       
         CLC   BAOFR,SPACES        NO AGENCY OF RECORD                          
         BNH   CKVEN20                                                          
         TM    BACTL,X'80'         PUB TRANSLATION REQUIRED                     
         BZ    CKVEN20             NO                                           
*                                                                               
* INPUT MUST BE A PUB ON AGY OF REC FILE                                        
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,1                MISSING FIELD                                
         B     ERROR                                                            
*                                                                               
* NOTE USE OF WORK                                                              
*                                                                               
         GOTO1 VPUBVAL,DMCB,(5(R2),CLIDES),(0,WORK)                             
         CLI   DMCB,X'FF'                                                       
         BNE   CKVEN2                                                           
CKVENERR LA    R3,PUBERR                                                        
         B     ERROR                                                            
*                                                                               
CKVEN2   MVC   SAVEKEY,KEY         SAVE KEY AND DMWORK                          
         MVC   DMWORK1(96),DMWORK                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BMED                                                      
         MVC   KEY+1(6),WORK                                                    
         MVC   KEY+7(2),BAOFR      AGY OF REC                                   
         MVI   KEY+9,X'81'                                                      
         BRAS  RE,READPUB                                                       
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BRAS  RE,GETPUB                                                        
         L     R9,SAVERE           RESTORE R9                                   
*                                                                               
         FOUT  CLIPUBNH,PUBIO2+35,20                                            
*                                                                               
* NOW CHECK FOR SPECIAL CLIENT ELEMENTS, ERROR IF FOUND ANY                     
*                                                                               
         LA    R7,PUBIO2+33                                                     
CKVEN2A  CLI   0(R7),0                                                          
         BE    CKVEN2E             PUB NOT AUTHORIZED                           
         CLI   0(R7),X'14'                                                      
         BNE   CKVEN2C                                                          
         CLC   2(3,R7),BCLT                                                     
         BNE   CKVEN2C                                                          
         B     CKVEN2X                                                          
*                                                                               
CKVEN2C  SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     CKVEN2A                                                          
*                                                                               
CKVEN2E  LA    R3,CLTAUTH                                                       
         B     ERROR                                                            
*                                                                               
CKVEN2X  DS    0H                                                               
         LA    R7,PUBIO2+33                                                     
CKVEN3   CLI   0(R7),0                                                          
         BE    CKVEN6                                                           
         CLI   0(R7),X'14'                                                      
         BNE   CKVEN4                                                           
         CLI   2(R7),X'FF'                                                      
         BNE   CKVEN4                                                           
         CLC   3(2,R7),AGYALPHA                                                 
         BNE   CKVEN4                                                           
         CLC   17(6,R7),PUBIO+1    SAME AS THIS PUB - OK                        
         BE    CKVEN6X                                                          
         LA    R3,PUBERR                                                        
         B     ERROR               AGY OF RECORD  PUB  HAS AN ASSGN             
*                                                                               
CKVEN4   SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     CKVEN3                                                           
*                                                                               
* NO ASSGN FOUND NEED TO ADD ONE                                                
*                                                                               
CKVEN6   XC    ELEAREA+100(50),ELEAREA+100                                      
         LA    R6,ELEAREA+100                                                   
         USING LTLREPD,R6                                                       
         MVC   0(2,R6),=X'142D'    NEW LENGTH OF 45                             
         MVC   2(1,R6),=X'FF'                                                   
         MVC   3(2,R6),AGYALPHA                                                 
         MVC   PUBCVEN(6),PUBIO+1  PUB NUMBER                                   
*                                                                               
* ADD ELEMENT                                                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PUBIO2),LTLREPD,0(R7)                             
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BRAS  RE,PUTPUB                                                        
         L     R9,SAVERE           RESTORE R9                                   
         DROP  R6                                                               
*                                                                               
* SEE IF IT WAS ASSGNED TO ANOTHER PUB, IF SO NEED TO DELETE ELEM               
*                                                                               
         USING LTLREPD,R4                                                       
*                                                                               
CKVEN6X  OC    SPUBCVEN,SPUBCVEN                                                
         BZ    CKVEN8              DONE                                         
         CLI   SPUBCVEN,X'99'      SEE IF IT WAS A PUB NUMBER                   
         BH    CKVEN8              NO - DONE                                    
*                                                                               
         CLC   SPUBCVEN(6),WORK    NO CHANGE IN ASSGNS                          
         BE    CKVEN8              SO DON'T DELETE ELEM                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BMED                                                      
         MVC   KEY+1(6),SPUBCVEN                                                
         MVC   KEY+7(2),BAOFR      AGY OF REC                                   
         MVI   KEY+9,X'81'                                                      
         BRAS  RE,HIGHPUB                                                       
         CLC   KEY(10),KEYSAVE                                                  
         BNE   CKVEN8              NOT FOUND  - DONE                            
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BRAS  RE,GETPUB                                                        
         L     R9,SAVERE                                                        
*                                                                               
         LA    R7,PUBIO2+33                                                     
*                                                                               
CKVEN7   CLI   0(R7),0                                                          
         BE    CKVEN8              NO ASSGN ELEM - DONE                         
         CLI   0(R7),X'14'                                                      
         BNE   CKVEN7A                                                          
         CLI   2(R7),X'FF'                                                      
         BNE   CKVEN7A                                                          
         CLC   3(2,R7),AGYALPHA                                                 
         BNE   CKVEN7A                                                          
         CLC   17(6,R7),PUBIO+1    ASSGN MUST HAVE BEEN TO THIS PUB             
         BE    CKVEN7B                                                          
         DC    H'0'                FATAL ERROR                                  
*                                                                               
CKVEN7A  SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     CKVEN7                                                           
*                                                                               
* DELETE ELEM                                                                   
*                                                                               
CKVEN7B  GOTO1 VRECUP,DMCB,(1,PUBIO2),0(R7),0                                   
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BRAS  RE,PUTPUB                                                        
         L     R9,SAVERE                                                        
*                                                                               
CKVEN8   MVC   KEY,SAVEKEY         RESTORE KEY AND DMWORK                       
         MVC   DMWORK(96),DMWORK1                                               
         XC    PUBCVEN,PUBCVEN                                                  
         MVC   PUBCVEN(6),WORK     WORK HAS NEW ASSGN                           
         B     CKVEN20E                                                         
*                                                                               
CKVEN20  DS    0H                                                               
         CLI   5(R2),12            MAX 12 CHARS                                 
         BNH   CKVEN20A                                                         
CKVENER1 LA    R3,FLDINVER                                                      
         B     ERROR                                                            
*                                                                               
CKVEN20A CLI   5(R2),4                                                          
         BNE   CKVEN20B                                                         
         CLC   CLIDES(4),=C'NONE'                                               
         BNE   CKVEN20B                                                         
         XC    PUBCVEN,PUBCVEN                                                  
         MVI   PUBCVEN,X'FF'                                                    
         B     CKVEN20E                                                         
*                                                                               
CKVEN20B MVC   PUBCVEN,CLIDES                                                   
         OC    PUBCVEN,PUBCVEN                                                  
         BZ    CKVEN20X                                                         
         CLI   PUBCVEN,X'99'                                                    
         BL    CKVENER1                                                         
*                                                                               
CKVEN20E MVI   ESWITCH,1                                                        
*                                                                               
CKVEN20X DS    0H                  DONE WITH CLIENT VENDOR NUMBER               
*                                                                               
         FOUT   CLICOM1H                                                        
         CLI    CLICOM1H+5,0                                                    
         BE     EDWHR1             EDIT WHERE COMMENTS ARE TO GO                
         LA     R2,CLICOM1H                                                     
*                                                                               
         CLC    BCLT,=3X'FF'       DON'T ALLOW FOR ALL CLIENTS                  
         BE     ALLCERR                                                         
         BRAS   RE,CHKOUT          SEE IF COMMENT EXITST                        
         BE     *+12                                                            
         LA     R3,RECNFDER                                                     
         B      ERROR                                                           
*                                                                               
         MVC    WORK+10(20),SPACES RIGHT JUSTIFY                                
         ZIC    RF,CLICOM1H+5                                                   
         BCTR   RF,0                                                            
         EX     RF,*+8                                                          
         B      *+10                                                            
         MVC    WORK+15(0),CLICOM1                                              
         OC     WORK+15(6),SPACES                                               
         LA     RF,WORK+21                                                      
         CLI    0(RF),X'40'                                                     
         BH     *+10                                                            
         BCTR   RF,0                                                            
         B      *-10                                                            
         SHI    RF,5                                                            
         MVC    PUBCSC1,0(RF)                                                   
         MVI    ESWITCH,1                                                       
*                                                                               
EDWHR1   FOUT   CLIWHR1H                                                        
         CLI    CLIWHR1H+5,0                                                    
         BNE    WHR1EDT                                                         
         CLI    CLICOM1H+5,0                                                    
         BE     CHKNEXT                                                         
         LA     R2,CLIWHR1H                                                     
         B      MISSING                                                         
*                                                                               
MISSING1 LA     R2,CLICOM1H                                                     
MISSING  LA     R3,1                                                            
         B      ERROR                                                           
*                                                                               
WHR1EDT  CLI    CLICOM1H+5,0       IF STD COMMENT 1 IS 0-ERROR                  
         BE     MISSING1                                                        
*                                                                               
         CLI    CLIWHR1,C'I'       INSERTION ORDERS FOR NOW                     
         MVI    PUBCSCC1,X'80'                                                  
         BE     CHKNEXT                                                         
         LA     R2,CLIWHR1H                                                     
         LA     R3,2               INVALID INPUT                                
         B      ERROR                                                           
*                                                                               
CHKNEXT  DS    0H                                                               
         FOUT   CLICOM2H                                                        
         CLI    CLICOM2H+5,0                                                    
         BE     EDWHR2             EDIT WHERE COMMENTS ARE TO GO                
         LA     R2,CLICOM2H                                                     
         CLC    BCLT,=3X'FF'       DON'T ALLOW FOR ALL CLIENTS                  
         BE     ALLCERR                                                         
         CLI    CLICOM1H+5,0       NOTHING MUST BE IN FIRST SET                 
         LA     R2,CLICOM1H        WHERE CURSOR IS TO BE POSITIONED             
         BE     MISSING            EDIT WHERE COMMENTS ARE TO GO                
         LA     R2,CLICOM2H                                                     
*                                                                               
         MVC    WORK+10(20),SPACES RIGHT JUSTIFY                                
         ZIC    RF,CLICOM2H+5                                                   
         BCTR   RF,0                                                            
         EX     RF,*+8                                                          
         B      *+10                                                            
         MVC    WORK+15(0),CLICOM2                                              
         OC     WORK+15(6),SPACES                                               
         LA     RF,WORK+21                                                      
         CLI    0(RF),X'40'                                                     
         BH     *+10                                                            
         BCTR   RF,0                                                            
         B      *-10                                                            
         SHI    RF,5                                                            
         MVC    PUBCSC2,0(RF)                                                   
         MVI    ESWITCH,1                                                       
*                                                                               
         BRAS   RE,CHKOUT          SEE IF COMMENT EXITST                        
         BE     *+12                                                            
         LA     R3,RECNFDER                                                     
         B      ERROR                                                           
*                                                                               
EDWHR2   FOUT   CLIWHR2H                                                        
         CLI    CLIWHR2H+5,0                                                    
         BNE    WHR2EDT                                                         
         CLI    CLICOM2H+5,0                                                    
         BE     NONEXT                                                          
         LA     R2,CLIWHR2H                                                     
         B      MISSING                                                         
*                                                                               
WHR2EDT  CLI    CLICOM2H+5,0       IF STD COMMENT 2 IS 0-ERROR                  
         LA     R2,CLICOM2H                                                     
         BE     MISSING                                                         
*                                                                               
         MVI    PUBCSCC2,X'80'                                                  
         LA     R2,CLIWHR2H                                                     
         CLI    CLIWHR2,C'I'       INSERTION ORDERS FOR NOW                     
         BE     NONEXT                                                          
         LA     R3,2               INVALID INPUT                                
         B      ERROR                                                           
*                                                                               
NONEXT   DS    0H                                                               
*                                                                               
POCR     DS    0H                  PAY ONLY IF CASH RECEIVED                    
         MVI   PUBCCTL,0                                                        
         LA    R2,CLIPOCRH                                                      
         CLI   5(R2),0                                                          
         BE    POCRX                                                            
*                                                                               
         CLI   5(R2),1             ACCEPT N OR Y; NO OR YES                     
         BNE   POCR5                                                            
         CLI   8(R2),C'N'                                                       
         BE    POCRX                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   POCRERR                                                          
         MVI   PUBCCTL,X'01'                                                    
         MVI   ESWITCH,1                                                        
         B     POCRX                                                            
*                                                                               
POCRERR  DS     0H                                                              
         LA     R3,2               INVALID INPUT                                
         B      ERROR                                                           
*                                                                               
POCR5    DS    0H                  MORE THAN 1 CHARACTER INPUT                  
         CLI   5(R2),3                                                          
         BH    POCRERR                                                          
         CLC   8(2,R2),=C'NO'                                                   
         BE    POCRX                                                            
         CLC   8(3,R2),=C'YES'                                                  
         BNE   POCRERR                                                          
         MVI   PUBCCTL,X'01'                                                    
         MVI   ESWITCH,1                                                        
         B     POCRX                                                            
*                                                                               
POCRX    DS    0H                                                               
         EJECT                                                                  
*                                                                               
PAYOP1   DS    0H                  FIRST PAY OPTION                             
         MVI   PUBPCTL1,0                                                       
         LA    R2,CLIPO1H                                                       
         CLI   5(R2),0                                                          
         BE    PAYOP1X                                                          
*                                                                               
         CLI   5(R2),1             ACCEPT N OR Y; NO OR YES                     
         BNE   PAYOP15                                                          
         MVI   PUBPCTL1,C'N'                                                    
         CLI   8(R2),C'N'                                                       
         BE    PAYOP1X                                                          
         MVI   PUBPCTL1,C'Y'                                                    
         CLI   8(R2),C'Y'                                                       
         BE    PAYOP1X                                                          
*                                                                               
PAYOP1E  DS    0H                                                               
         LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
*                                                                               
PAYOP15  DS    0H                  MORE THAN 1 CHARACTER INPUT                  
         CLI   5(R2),3                                                          
         BH    PAYOP1E                                                          
         MVI   PUBPCTL1,C'N'                                                    
         CLC   8(2,R2),=C'NO'                                                   
         BE    PAYOP1X                                                          
         CLC   8(3,R2),=C'YES'                                                  
         BNE   PAYOP1E                                                          
         MVI   PUBPCTL1,C'Y'                                                    
         MVI   ESWITCH,1                                                        
         B     PAYOP1X                                                          
*                                                                               
PAYOP1X  DS    0H                                                               
*                                                                               
PAYOP2   DS    0H                  $PAY MATCHED CONTROL                         
         MVI   PUBPCTL2,0                                                       
         LA    R2,CLIPO2H                                                       
         CLI   5(R2),0                                                          
         BE    PAYOP2X                                                          
*                                                                               
         CLI   8(R2),C'N'          ACCEPT N,Y,O                                 
         BE    PAYOP2F                                                          
         CLI   8(R2),C'Y'                                                       
         BE    PAYOP2F                                                          
         CLI   8(R2),C'O'                                                       
         BE    PAYOP2F                                                          
*                                                                               
PAYOP2E  DS    0H                                                               
         LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
*                                                                               
PAYOP2F  MVC   PUBPCTL2,8(R2)                                                   
         MVI   ESWITCH,1                                                        
PAYOP2X  DS    0H                                                               
*                                                                               
PAYOP3   DS    0H                  $PAY VIA $MAT CONTROL                        
         MVI   PUBPCTL3,0                                                       
         LA    R2,CLIPO3H                                                       
         CLI   5(R2),0                                                          
         BE    PAYOP3X                                                          
*                                                                               
         CLI   8(R2),C'N'          ACCEPT N,Y,O,I                               
         BE    PAYOP3F                                                          
         CLI   8(R2),C'Y'                                                       
         BE    PAYOP3F                                                          
         CLI   8(R2),C'O'                                                       
         BE    PAYOP3F                                                          
         CLI   8(R2),C'I'                                                       
         BE    PAYOP3F                                                          
*                                                                               
PAYOP3E  DS    0H                                                               
         LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
*                                                                               
PAYOP3F  MVC   PUBPCTL3,8(R2)                                                   
         MVI   ESWITCH,1                                                        
PAYOP3X  DS    0H                                                               
*                                                                               
         B     UPDATER                                                          
*                                                                               
ALLCERR  LA    R3,FLDINVER         NO STND COMMENTS FOR 'ALL' CLT SCR           
         B     ERROR                                                            
*                                                                               
FINDREL  LA    R4,PUBREC+33                                                     
         CLI   0(R4),0                                                          
         BNE   CKREL1                                                           
         MVI   ASWITCH,1                                                        
         B     EDITR                                                            
*                                                                               
CKREL1   CLI   0(R4),X'14'                                                      
         BNE   NEXTR                                                            
         CLC   2(3,R4),BCLT                                                     
         BNE   NEXTR                                                            
         B     EDITR                                                            
*                                                                               
NEXTR    SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CKREL1                                                           
         MVI   ASWITCH,1                                                        
         LA    R4,ELEAREA          BUILD REP ELEM IN ELEM AREA                  
         B     EDITR                                                            
*                                                                               
* ASWITCH=1 IF PUBREPEL DID NOT EXIST                                           
* RECUP TO ADD IT IF ESWITCH=1 OR DELETE IT IF ESWITCH=0                        
*                                                                               
UPDATER  DS    0H                                                               
*                                                                               
UPDATE1  CLI   ESWITCH,0                                                        
         BE    EDITX                                                            
UPDATE2  MVC   PUBREPEL(2),=X'1435'                                             
         MVC   PUBRPOFF(3),BCLT                                                 
         LA    R5,PUBREC+33        SET R5 TO WHERE  I WANT TO ADD ELEM          
         CR    R5,R4               WILL BE EQUAL IF REC HAD NO ELEMS            
         BE    UPDATE4             GO ADD ELEM                                  
*                                                                               
UPDATE3  CLI   0(R5),0                                                          
         BE    UPDATE4                                                          
         CLI   0(R5),X'14'                                                      
         BNE   NEXTR1                                                           
         CLC   2(3,R5),BCLT                                                     
         BH    UPDATE4                                                          
*                                                                               
NEXTR1   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     UPDATE3                                                          
UPDATE4  GOTO1 VRECUP,DMCB,(1,PUBREC),LTLREPD,0(R5)                             
         B     EDITX                                                            
*                                                                               
DELETER  CLI   ASWITCH,1                                                        
         BE    EDITX                                                            
         GOTO1 VRECUP,DMCB,(1,PUBREC),LTLREPD                                   
EDITX    DS    0H                                                               
*                                                                               
WRITEIT  OC    PUBADDR,PUBADDR                                                  
         BZ    DONE                                                             
         MVC   KEY+27(4),PUBADDR                                                
         BRAS  RE,PUTPUB                                                        
*                                                                               
         BRAS  RE,CKX15ELM         CK FOR FIELDS IN X15 ELEM                    
         BE    WRI_X                                                            
         L     R2,WKFULL2          GET ADDRESS OF FLD IN ERROR                  
         B     EXIT                                                             
*                                                                               
WRI_X    B     DONE                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'0A'                                                    
         BNE   FMT2                                                             
         CLI   BACT,B$ADD                                                       
         BNE   FMT5                                                             
         MVI   BYTE2,0             SWITCH TO EDIT MODE                          
         B     EDIT                                                             
*                                                                               
FMT2     LA    R6,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90406FA'                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SAVSCRN,X'0A'                                                    
*                                                                               
FMT5     BRAS  RE,CLR_FLDS                                                      
*                                                                               
         BRAS  RE,DSP_SUBM         DISPLAY SUBMEDIA ELEM DATA                   
*                                                                               
         TM    LTLIND,X'30'                                                     
         BNZ   FMT8                                                             
         LA    R2,CLIPREPH                                                      
         B     EXIT                                                             
*                                                                               
FMT8     BRAS  RE,PUT_FLDS                                                      
*                                                                               
DONE     MVI   BYTE3,1                                                          
*                                                                               
         BRAS  RE,INITWKST         INITIALIZE WORKING STORAGE                   
*                                                                               
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTELEM  SR    R0,R0                                                            
         IC    R0,1(R5)            R5 POINTS TO FIRST BUY RECORD ELEM           
         AR    R5,R0               FIRST ELEM IS ALWAYS X'10'                   
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_ETXT LR    R0,RE               SAVE RETURN ADDRESS                          
         MVI   ERRAREA,X'FF'                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ANY      CLI   5(R2),0                                                          
         JNE   ANY_20                                                           
         LHI   R3,MISSERR                                                       
ANY_10   STH   R3,ERRNUM_H                                                      
         ST    RE,SAVERE                                                        
         GOTOR GET_ETXT                                                         
         L     RE,SAVERE                                                        
         LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
*                                                                               
ANY_20   TM    4(R2),X'10'         VALID FIELD (IF APPLICABLE)?                 
         BCR   8,RE                                                             
         LHI   R3,FLDINVER                                                      
         J     ANY_10                                                           
*                                                                               
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
READ     MVC   COMMAND,DMGR_REA                                                 
         MVC   KEYSAVE,KEY                                                      
         J     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,DMGR_SEQ                                                 
         J     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,DMGR_HIG                                                 
         MVC   KEYSAVE,KEY                                                      
         J     DIRCTRY                                                          
*                                                                               
ADD      MVC   COMMAND,DMGR_ADD                                                 
         J     DIRCTRY                                                          
*                                                                               
WRITE    MVC   COMMAND,DMGR_WRI                                                 
         J     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),PPRT_DIR,               X        
               KEY,KEY,(TERMNAL,0)                                              
         J     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREC   MVC   COMMAND,DMGETREC                                                 
         J     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,DMPUTREC                                                 
         J     FILE                                                             
*                                                                               
ADDREC   MVC   COMMAND,DMADDREC                                                 
         J     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND,DMGR_DEL                                                 
         JE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),PPRT_FIL,               X        
               (R2),AIO_DEF,(TERMNAL,DMWORK)                                    
         J     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
READPUB  MVC   COMMAND,DMGR_REA                                                 
         MVC   KEYSAVE,KEY                                                      
         J     PUBDIRY                                                          
*                                                                               
SEQPUB   MVC   COMMAND,DMGR_SEQ                                                 
         J     PUBDIRY                                                          
*                                                                               
HIGHPUB  MVC   COMMAND,DMGR_HIG                                                 
         MVC   KEYSAVE,KEY                                                      
         J     PUBDIRY                                                          
*                                                                               
WRITEPUB MVC   COMMAND,DMGR_WRI                                                 
         J     PUBDIRY                                                          
*                                                                               
ADDPUBD  MVC   COMMAND,DMGR_ADD                                                 
         J     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),PPUB_DIR,               X        
               KEY,KEY,(TERMNAL,0)                                              
         J     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBFILE)                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETPUB   MVC   COMMAND,DMGETREC                                                 
         J     PUBFILE                                                          
*                                                                               
PUTPUB   MVC   COMMAND,DMPUTREC                                                 
         J     PUBFILE                                                          
*                                                                               
ADDPUB   MVC   COMMAND,DMADDREC                                                 
         J     PUBFILE                                                          
*                                                                               
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),PPUB_FIL,               X        
               (R2),PUBIO,(TERMNAL,DMWORK)                                      
         J     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         JNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3               LET GETMSG SORT IT OUT                       
         J     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         MVI   WKELEMSW,0                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGE                   
*                                                                               
         LA    RE,IOAREA                                                        
         ST    RE,AIO_DEF          DEFAULT IO AREA                              
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO1-PPPUB0AD)                                           
         ST    RE,AWKAIO1                                                       
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVC   PPRT_DIR,=C'PRTDIR'                                              
         MVC   PPRT_FIL,=C'PRTFILE'                                             
         MVC   PPUB_DIR,=C'PUBDIR'                                              
         MVC   PPUB_FIL,=C'PUBFILE'                                             
*                                                                               
         MVC   DMGR_REA,=C'DMREAD'                                              
         MVC   DMGR_SEQ,=C'DMRSEQ'                                              
         MVC   DMGR_HIG,=C'DMRDHI'                                              
         MVC   DMGR_ADD,=C'DMADD '                                              
         MVC   DMGR_WRI,=C'DMWRT '                                              
         MVC   DMGR_DEL,=C'DMDEL '                                              
*                                                                               
         MVC   DMGETREC,=C'GETREC'                                              
         MVC   DMPUTREC,=C'PUTREC'                                              
         MVC   DMADDREC,=C'ADDREC'                                              
*                                                                               
         XCEFL ELEAREA,500                                                      
         XCEFL PUBIO,4000                                                       
         XCEFL PUBIO2,4000                                                      
*                                                                               
         MVI   WKELEMSW,0          ELEM EDITED SWITCH                           
         XC    SVSUBMED,SVSUBMED                                                
         MVI   SVPAYNCD,X'FF'      SET TO "BLANK"                               
         MVI   SVPAY_CD,X'FF'      SET TO "BLANK"                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
XXIT     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PACK     NTR1  BASE=*,LABEL=*      RETURNS VALUE IN DUB AND R0                  
*                                                                               
         LA    RE,PACK_X                                                        
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACK_X              EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BZ    PACK_X              OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
*                                                                               
PACK_X   XIT1  REGS=(R0)           R0 HAS BINARY VALUE                          
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLR_FLDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         FOUT  CLIPREPH,SPACES,4                                                
         FOUT  CLITREPH,SPACES,4                                                
         FOUT  CLICREPH,SPACES,4                                                
         FOUT  CLIDESH,SPACES,15                                                
         FOUT  CLICOM1H,SPACES,6                                                
         FOUT  CLICOM2H,SPACES,6                                                
         FOUT  CLIWHR1H,SPACES,1                                                
         FOUT  CLIWHR2H,SPACES,1                                                
         FOUT  CLIPRNH,SPACES,30   NAMES                                        
         FOUT  CLITRNH,SPACES,30                                                
         FOUT  CLICRNH,SPACES,30                                                
         FOUT  CLIPUBNH,SPACES,30                                               
         FOUT  CLIPOCRH,SPACES,4                                                
*                                                                               
         FOUT  CLIPO1H,SPACES,4    NEW PAY OPTIONS                              
         FOUT  CLIPO2H,SPACES,1                                                 
         FOUT  CLIPO3H,SPACES,1                                                 
*                                                                               
         XC    CLISUBM,CLISUBM                                                  
         OI    CLISUBMH+6,X'80'                                                 
*                                                                               
         XC    CLISMDS,CLISMDS                                                  
         OI    CLISMDSH+6,X'80'                                                 
*                                                                               
         XC    CLIINCD,CLIINCD                                                  
         OI    CLIINCDH+6,X'80'                                                 
*                                                                               
         XC    CLIICD,CLIICD                                                    
         OI    CLIICDH+6,X'80'                                                  
*                                                                               
         J     XXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKOUT   NTR1  BASE=*,LABEL=*      VERIFY EXISTANCE OF STD COMMENT              
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
ED15     MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),SPACES                                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    ED16                NO INPUT // NO CHECKING                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1                                                            
         AHI   RF,6                                                             
         LA    RF,KEY+4(RF)                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)       MOVE TO KEY+4+(6-L)                          
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ED16                                                             
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         BRAS  RE,HIGH             RESTORE D/M POINTERS                         
*                                                                               
         J     SETCCNEQ            ERROR PATH FORCE BNE                         
*                                                                               
ED16     DS    0H                  NON-ERROR PATH // FORE BE                    
         MVC   KEY,SAVEKEY                                                      
         BRAS  RE,HIGH             RESTORE D/M POINTERS                         
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CC NOT EQUAL - WKFULL2 WILL RETURN ADDRESS OF FLD IN ERROR                    
*                ERROR MSG WILL BE SET IN HEADER                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKX15ELM NTR1  BASE=*,LABEL=*      CK SUBMEDIA FIELDS ARE ENTERED               
*                                                                               
         XC    ERRNUM_H,ERRNUM_H                                                
*                                                                               
         LA    R2,CLISUBMH                                                      
         GOTOR CKSUBMED            GO VALIDATE SUBMEDIA CODE                    
         BNE   CKX15_ER                                                         
*                                                                               
         LA    R2,CLIINCDH         NOT CASH DISCOUNTED                          
         GOTOR CKDTATIV            GO VALIDATE DAYS TO ADD TO INVOICE           
         BNE   CKX15_ER                                                         
         MVC   SVPAYNCD,WKBYTE2    SAVE PAY NCD FOR ELEM                        
*                                                                               
         LA    R2,CLIICDH          CASH DISCOUNTED                              
         GOTOR CKDTATIV            GO VALIDATE DAYS TO ADD TO INVOICE           
         BNE   CKX15_ER                                                         
         MVC   SVPAY_CD,WKBYTE2    SAVE PAY CD FOR ELEM                         
*                                                                               
         LA    R2,CLIINCDH         NOT CASH DISCOUNTED                          
         CLI   SVPAYNCD,X'FF'      NCD VALUE IS ENTERED?                        
         BNE   CKX15_30                                                         
         CLI   SVPAY_CD,X'FF'      CD VALUE IS ENTERED?                         
         BE    CKX15_30                                                         
CKX15_20 LHI   R3,MISSERR                                                       
         STH   R3,ERRNUM_H                                                      
         B     CKX15_ER            NCD/CD, IF ENTERED, BOTH REQUIRED            
*                                                                               
CKX15_30 LA    R2,CLIICDH          CASH DISCOUNTED                              
         CLI   SVPAY_CD,X'FF'      CD VALUE IS ENTERED?                         
         BNE   *+16                                                             
         CLI   SVPAYNCD,X'FF'      NCD VALUE IS ENTERED?                        
         BE    *+8                                                              
         B     CKX15_20                                                         
*                                                                               
         LA    R2,PBLMEDH                                                       
         BRAS  RE,CKELEMSW         CK IF MORE ELEMS NEED TO BE ADDED            
         BNE   CKX15_ER                                                         
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
CKX15_ER ST    R2,WKFULL2          RETURN ADDRESS OF FLD IN ERROR               
         LH    R3,ERRNUM_H                                                      
         GOTOR GET_ETXT                                                         
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSUBMED NTR1  BASE=*,LABEL=*      VALIDATE SUBMEDIA CODE                       
*                                                                               
         XC    CLISMDS,CLISMDS     CLR SUBMEDIA DESCRIPTION                     
         OI    CLISMDSH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0             INPUT?                                       
         JE    SETCCEQ                                                          
*                                                                               
         OI    9(R2),C' '                                                       
         CLI   9(R2),C' '                                                       
         BE    *+14                                                             
         MVC   ERRNUM_H,=AL2(TOOLNGER)                                          
         J     SETCCNEQ                                                         
*                                                                               
         MVC   WKBYTE2,8(R2)       SUBMEDIA CODE TO BE VALIDATED                
         BRAS  RE,GETSUBMD                                                      
         BE    *+14                                                             
         MVC   ERRNUM_H,=AL2(RECNFDER)                                          
         J     SETCCNEQ                                                         
*                                                                               
         OI    WKELEMSW,SUBMDELQ   NEED TO BUILD SUBMEDIA ELEM                  
         MVC   SVSUBMED,8(R2)      SAVE SUBMEDIA CODE FOR ELEM                  
*                                                                               
         XC    CLISMDS,CLISMDS                                                  
         MVC   CLISMDS,SVSMDESP                                                 
         OI    CLISMDSH+6,X'80'                                                 
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKDTATIV NTR1  BASE=*,LABEL=*      VALIDATE DAYS TO ADD TO INVOICE              
*                                                                               
         MVI   WKBYTE2,X'FF'       SET RETURN VALUE TO "BLANK"                  
         OI    WKELEMSW,SUBMDELQ   NEED TO BUILD SUBMEDIA ELEM                  
*                                                                               
         OC    8(L'CLIINCD,R2),SPACES                                           
         CLC   8(L'CLIINCD,R2),SPACES                                           
         JE    SETCCEQ                                                          
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BO    *+14                                                             
         MVC   ERRNUM_H,=AL2(NOTNUMER)                                          
         J     SETCCNEQ                                                         
*                                                                               
         BRAS  RE,PACK                                                          
         CHI   R0,90                                                            
         BNH   *+14                                                             
         MVC   ERRNUM_H,=AL2(MAXEXERR)                                          
         J     SETCCNEQ                                                         
*                                                                               
         STC   R0,WKBYTE2          RETURN VALIDATED NUMBER                      
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTREP   NTR1  BASE=*,LABEL=*      REP NAME SEARCHING                           
*                                                                               
         ST    R2,FULL                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,BMED                                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'REP'),0,RR=RELO0A                       
         L     R2,FULL                                                          
*                                                                               
         LHI   R3,REPERR                                                        
         CLI   8(R2),C'A'                                                       
         BNE   EDTREP2                                                          
         CLI   5(R2),4                                                          
         JNE   ERROR                                                            
         MVC   KEY+4(4),8(R2)                                                   
         B     EDTREP4                                                          
*                                                                               
EDTREP2  BRAS  RE,ANY                                                           
         JNE   SETCCNEQ            ERROR IS SET IN HEADER                       
*                                                                               
         BRAS  RE,PACK                                                          
*                                                                               
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+4(4),DUB+5(3)                                                
EDTREP4  MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'11'                                                      
         BRAS  RE,READ                                                          
         MVC   DMWORK1(96),DMWORK                                               
         BRAS  RE,GETREC                                                        
         MVC   DMWORK(96),DMWORK1                                               
         MVC   8(4,R2),KEY+4                                                    
         MVC   KEY(32),SAVEKEY                                                  
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUT_FLDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    LTLIND,X'10'        NO ELEMENT FOUND                             
         BNO   PUTVENXX                                                         
         LA    R4,PUBREC+33                                                     
PUTFLD1  CLI   0(R4),X'14'                                                      
         BNE   NEXT1                                                            
         CLC   2(3,R4),BCLT                                                     
         BNE   NEXT1                                                            
         USING LTLREPD,R4                                                       
         LA    R2,CLIPREPH                                                      
         LA    R5,CLIPRNH                                                       
         FOUT  CLIPRNH,SPACES,30   NAMES                                        
         FOUT  CLITRNH,SPACES,30                                                
         FOUT  CLICRNH,SPACES,30                                                
         FOUT  CLIPUBNH,SPACES,30                                               
         OC    PUBPAREP,PUBPAREP                                                
         BZ    PUTREP1                                                          
         FOUT  CLIPREPH,PUBPAREP,4                                              
         BRAS  RE,PUTRNAME                                                      
PUTREP1  LA    R2,CLITREPH                                                      
         LA    R5,CLITRNH                                                       
         OC    PUBTRREP,PUBTRREP                                                
         BZ    PUTREP2                                                          
         FOUT  CLITREPH,PUBTRREP,4                                              
         BRAS  RE,PUTRNAME                                                      
PUTREP2  LA    R2,CLICREPH                                                      
         LA    R5,CLICRNH                                                       
         OC    PUBCNREP,PUBCNREP                                                
         BZ    PUTVEN                                                           
         FOUT  CLICREPH,PUBCNREP,4                                              
         BRAS  RE,PUTRNAME                                                      
         B     PUTVEN                                                           
*                                                                               
PUTVEN   DS    0H                  IF OLD ELEM, DO NOT DISPLAY IT               
         CLI   1(R4),32            OLD LENGTH?                                  
         BE    PUTVENXX                                                         
         CLI   PUBCSCC1,0                                                       
         BE    NOCNTRL1                                                         
         TM    PUBCSCC1,X'80'                                                   
         BZ    NOCNTRL1                                                         
         MVI   CLIWHR1,C'I'                                                     
         FOUT  CLIWHR1H                                                         
*                                                                               
NOCNTRL1 DS    0H                                                               
         CLI   PUBCSCC2,0                                                       
         BE    NOCNTRL2                                                         
         TM    PUBCSCC2,X'80'                                                   
         BZ    NOCNTRL2                                                         
         MVI   CLIWHR2,C'I'                                                     
         FOUT  CLIWHR2H                                                         
*                                                                               
NOCNTRL2 DS    0H                                                               
         OC    PUBCSC1,PUBCSC1                                                  
         BZ    NOCOMM1                                                          
         FOUT  CLICOM1H,PUBCSC1,6                                               
*                                                                               
NOCOMM1  DS    0H                                                               
         OC    PUBCSC2,PUBCSC2                                                  
         BZ    NOCOMM2                                                          
         FOUT  CLICOM2H,PUBCSC2,6                                               
*                                                                               
NOCOMM2  DS    0H                                                               
*                                                                               
PUTPAY   DS    0H                                                               
         MVC   CLIPOCR(2),=C'NO'                                                
         TM    PUBCCTL,X'01'                                                    
         BNO   *+10                                                             
         MVC   CLIPOCR(3),=C'YES'                                               
         FOUT  CLIPOCRH                                                         
*                                                                               
PUTPOS   DS    0H                                                               
         CLI   PUBREPEL+1,X'35'    FIRST CHECK ELEMENT LENGTH                   
         BL    PUTVENXX            IF LOW- - FIELDS WON'T BE PRESENT            
*                                                                               
         MVC   CLIPO1(3),=C'YES'                                                
         CLI   PUBPCTL1,C'Y'                                                    
         BE    PUTPOS3                                                          
         MVC   CLIPO1(3),=C'NO '                                                
         FOUT  CLIPO1H                                                          
*                                                                               
PUTPOS3  CLI   PUBPCTL2,C' '                                                    
         BNH   PUTPOS5                                                          
         MVC   CLIPO2,PUBPCTL2                                                  
         FOUT  CLIPO2H                                                          
*                                                                               
PUTPOS5  CLI   PUBPCTL3,C' '                                                    
         BNH   PUTPOSX                                                          
         MVC   CLIPO3,PUBPCTL3                                                  
         FOUT  CLIPO3H                                                          
*                                                                               
PUTPOSX  DS    0H                                                               
*                                                                               
PUTVENXX DS    0H                                                               
         XC    CLIPUBN,CLIPUBN                                                  
         XC    CLIDES,CLIDES                                                    
         TM    BACTL,X'80'                                                      
         BZ    PUTVEN4                                                          
*                                                                               
* SHOULD BE PUB NUMBER TRY TO DISPLAY AND READ PUB TO DISPLAY NAME              
*                                                                               
         MVC   SAVEKEY(32),KEY                                                  
         MVC   DMWORK1(96),DMWORK                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BMED                                                      
         MVC   KEY+1(6),PUBCVEN                                                 
         MVC   KEY+7(2),BAOFR      AGY OF REC                                   
         MVI   KEY+9,X'81'                                                      
         BRAS  RE,HIGHPUB                                                       
         CLC   KEYSAVE(10),KEY                                                  
         BE    PUTVEN1                                                          
         FOUT  CLIPUBNH,=C'** AGY OF REC PUB NOT FOUND **',30                   
*                                                                               
         CLI   PUBCVEN,X'99'       NOT A PUB NUMBER?                            
         BH    PUTVEN4                                                          
         B     PUTVEN2                                                          
*                                                                               
PUTVEN1  ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BRAS  RE,GETPUB                                                        
         L     R9,SAVERE                                                        
*                                                                               
         FOUT  CLIPUBNH,PUBIO2+35,20                                            
*                                                                               
PUTVEN2  DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         MVC   DMWORK(96),DMWORK1                                               
         IC    R5,APROF13                                                       
         GOTO1 VPUBEDIT,DMCB,((R5),PUBCVEN),(0,CLIDES)                          
         FOUT  CLIDESH                                                          
         B     PUTVENX                                                          
*                                                                               
PUTVEN4  CLI   PUBCVEN,X'FF'                                                    
         BNE   PUTVEN5                                                          
         FOUT  CLIDESH,=C'NONE',4                                               
         B     PUTVENX                                                          
*                                                                               
PUTVEN5  FOUT  CLIDESH,PUBCVEN,12                                               
*                                                                               
PUTVENX  CLI   BACT,B$CHA                                                       
         JH    XXIT                                                             
         LA    R2,CLIPREPH                                                      
         J     EXIT                                                             
*                                                                               
PUTRNAME ST    RE,SAVERE                                                        
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY                                                   
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),8(R2)                                                   
         BRAS  RE,HIGH                                                          
         MVC   DMWORK1(96),DMWORK                                               
         CLC   KEYSAVE(25),KEY                                                  
         BNE   NOREP                                                            
         BRAS  RE,GETREC                                                        
         FOUT  (R5),PREPNAME,30                                                 
         B     RETURN                                                           
*                                                                               
NOREP    XC    8(30,R5),8(R5)                                                   
         MVC   8(19,R5),=C'* REP NOT ON FILE *'                                 
RETURN   MVC   KEY(32),SAVEKEY                                                  
         MVC   DMWORK(96),DMWORK1                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
NEXT1    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    PUTVENXX                                                         
         B     PUTFLD1                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSP_SUBM NTR1  BASE=*,LABEL=*      DISPLAY SUBMEDIA ELEM DATA                   
*                                                                               
         LA    R5,PUBREC+33                                                     
         MVI   ELCODE,PUBPYELQ                                                  
         USING PUBPAYEL,R5                                                      
*                                                                               
D_SUBM20 BRAS  RE,NXTELEM                                                       
         BNE   D_SUBM_X                                                         
         CLC   PUBPYOFF,BCLT                                                    
         BNE   D_SUBM20                                                         
*                                                                               
         CLI   PUBPYSMD,0                                                       
         BE    D_SUBM30                                                         
*                                                                               
         MVC   CLISUBM(1),PUBPYSMD                                              
         OI    CLISUBMH+6,X'80'                                                 
*                                                                               
         MVC   WKBYTE2,PUBPYSMD                                                 
         BRAS  RE,GETSUBMD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLISMDS,SVSMDESP                                                 
         OI    CLISMDSH+6,X'80'                                                 
*                                                                               
D_SUBM30 CLI   PUBPYNCD,X'FF'                                                   
         BE    D_SUBM40                                                         
         EDIT  PUBPYNCD,CLIINCD,0,ALIGN=LEFT,ZERO=NOBLANK                       
         OI    CLIINCDH+6,X'80'                                                 
*                                                                               
D_SUBM40 CLI   PUBPY_CD,X'FF'                                                   
         BE    D_SUBM50                                                         
         EDIT  PUBPY_CD,CLIICD,0,ALIGN=LEFT,ZERO=NOBLANK                        
         OI    CLIICDH+6,X'80'                                                  
*                                                                               
D_SUBM50 DS    0H                  FOR FUTURE DATA                              
*                                                                               
D_SUBM_X J     XXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKELEMSW NTR1  BASE=*,LABEL=*      CK IF MORE ELEMS NEED TO BE ADDED            
*                                                                               
         TM    WKELEMSW,SUBMDELQ   SUBMEDIA ELEM NEED TO BE ADDED?              
         BZ    CKELSW_X                                                         
*                                                                               
         MVC   SAVEKEY,KEY         IN CASE THOSE FLDS NEEDED                    
         ST    R9,WKFULL2                                                       
*                                                                               
         OC    PUBADDR,PUBADDR                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+27(4),PUBADDR                                                
         LA    R9,PUBIO                                                         
         GOTOR GETPUB                                                           
*                                                                               
         MVI   ELCODE,PUBPYELQ                                                  
         USING PUBPAYEL,R5                                                      
         LA    R5,PUBIO+33                                                      
CKELSW20 BRAS  RE,NXTELEM                                                       
         BNE   CKELSW50            NOT FOUND, GO ADD ONE                        
*                                                                               
         CLC   PUBPYOFF,BCLT                                                    
         BE    CKELSW40                                                         
*                                                                               
         CLC   PUBPYOFF,BCLT                                                    
         BH    CKELSW50            INSERT NEW ELEM                              
         B     CKELSW20                                                         
*                                                                               
CKELSW40 GOTOR VRECUP,DMCB,(1,PUBIO),0(R5),0                                    
*                                                                               
CKELSW50 ST    R5,WKFULL1                                                       
         LA    R5,ELEAREA                                                       
         XC    ELEAREA(255),ELEAREA                                             
         MVI   PUBPYELC,PUBPYELQ                                                
         MVI   PUBPYELN,PUBPYLNQ                                                
         MVC   PUBPYOFF,BCLT                                                    
*                                                                               
         MVC   WKBYTE2,CLISUBM                                                  
         OI    WKBYTE2,C' '                                                     
         CLI   WKBYTE2,C' '                                                     
         BE    *+10                                                             
         MVC   PUBPYSMD,CLISUBM                                                 
*                                                                               
         MVC   PUBPYNCD,SVPAYNCD                                                
         MVC   PUBPY_CD,SVPAY_CD                                                
*                                                                               
         L     R5,WKFULL1                                                       
         GOTOR VRECUP,DMCB,(1,PUBIO),ELEAREA,0(R5)                              
*                                                                               
         XCEFL PUBIO2,4000         FOR COPYING PUBPYELQ ELEMS                   
         SR    R3,R3               FOR COUNTING RECORD SIZE                     
         LA    R4,PUBIO2           POINT TO FIRST ENTRY                         
*                                                                               
CKELSW60 LA    R5,PUBIO+33         COPY AND DELETE PUBPYELQ ELEMS               
         MVI   ELCODE,PUBPYELQ                                                  
         BRAS  RE,NXTELEM                                                       
         BNE   CKELSW70            NO MORE, GO RESEQUENCE ELEM                  
         CHI   R3,4000-33-PUBPYLNQ                                              
         BH    CKELSWER            CANNOT ADD TO RECORD ANYMORE                 
         SR    RE,RE                                                            
         IC    RE,PUBPYELN                                                      
         AR    R3,RE               ADD TO REC LENGTH COUNTER                    
         LR    RF,RE                                                            
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                BAD ELEM!                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PUBPAYEL                                                 
         AR    R4,RF               POINT TO NEXT SAVING SPOT                    
*                                                                               
         GOTOR VRECUP,DMCB,(1,PUBIO),0(R5),0                                    
*                                                                               
         B     CKELSW60                                                         
*                                                                               
CKELSW70 LA    R5,PUBIO+33                                                      
         MVI   ELCODE,X'14'        NEED TO GET TO END OF X'14' ELEMS            
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                 NO MORE, GO RESEQUENCE ELEM                  
         B     *-8                                                              
         LA    R4,PUBIO2           SAVED PUBPYELQ ELEMS                         
CKELSW76 CLI   0(R4),0             DONE RESEQUENCING PUBPYELQ ELEM?             
         BE    CKELSW80                                                         
         GOTOR VRECUP,DMCB,(1,PUBIO),0(R4),0(R5)                                
         SR    RE,RE                                                            
         IC    RE,1(R4)                                                         
         AR    R4,RE               POINT TO NEXT COPIED ENTRY                   
         AR    R5,RE               NEXT INSERTION POINT                         
         B     CKELSW76                                                         
*                                                                               
CKELSW80 GOTOR PUTPUB                                                           
*                                                                               
         MVC   KEY,SAVEKEY         RESTORED SAVED VALUES                        
         L     R9,WKFULL2                                                       
*                                                                               
CKELSW_X J     SETCCEQ                                                          
*                                                                               
CKELSWER MVC   ERRNUM_H,=AL2(RECMAXER)                                          
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* WKBYTE2    - SUBMEDIA CODE TO BE LOOKED UP                                    
* SVSMDESP   - SUBMEDIA DESCRIPTION TO BE RETURNED (CC IS EQUAL)                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETSUBMD NTR1  BASE=*,LABEL=*      GET SUBMEDIA                                 
*                                                                               
         XC    SVSMDESP,SVSMDESP   WILL RETURN SUBMEDIA DESCRIPTION             
         MVI   WKERRSW,0                                                        
         MVC   SAVEKEY,KEY                                                      
         MVC   WKSVAREC,AIO_DEF    SAVE DEFAULT IO POINTER                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING PSMDKEY,R5                                                       
         MVC   PSMDKAGY,AGYALPHA                                                
         MVC   PSMDKMED,BMED                                                    
         MVI   PSMDKRCD,PSMDKR1Q                                                
         MVI   PSMDKRC2,PSMDKR2Q                                                
         MVC   PSMDKSMD,WKBYTE2                                                 
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   WKERRSW,C'Y'        SUBMEDIA CODE IS NOT ON FILE                 
         B     GETSM90                                                          
*                                                                               
         MVC   AIO_DEF,AWKAIO1     USE WORKING AIO                              
         BRAS  RE,GETREC                                                        
         L     R5,AWKAIO1                                                       
         LA    R5,PSMDFRST-PSUBMEDR(R5)                                         
         CLI   0(R5),PSMDELCQ                                                   
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM IS NOT THERE                      
         USING PSMDELEM,R5                                                      
         MVC   SVSMDESP,PSMDDESC   SUBMEDIA DESCRIPTION                         
*                                                                               
GETSM90  CLI   WKERRSW,C'Y'                                                     
         JE    SETCCNEQ                                                         
         MVC   KEY,SAVEKEY                                                      
         BRAS  RE,HIGHPUB                                                       
         ST    R9,SAVERE                                                        
         L     R9,AIO_DEF                                                       
         BRAS  RE,GETPUB                                                        
         L     R9,SAVERE                                                        
         MVC   AIO_DEF,WKSVAREC                                                 
         J     SETCCEQ                                                          
*                                                                               
GETSM_X  J     XXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPPUB0AD DSECT                                                                  
*                                                                               
RELO0A   DS    F                                                                
ERRNUM_H DS    H                   HALF WORD ERROR NUMBER                       
AIO_DEF  DS    A                   DEFAULT AIO                                  
AWKAIO1  DS    A                                                                
SPACES   DS    CL132                                                            
*                                                                               
PPRT_DIR DS    CL6                                                              
PPRT_FIL DS    CL7                                                              
PPUB_DIR DS    CL6                                                              
PPUB_FIL DS    CL7                                                              
*                                                                               
DMGR_REA DS    CL(L'COMMAND)                                                    
DMGR_SEQ DS    CL(L'COMMAND)                                                    
DMGR_HIG DS    CL(L'COMMAND)                                                    
DMGR_ADD DS    CL(L'COMMAND)                                                    
DMGR_WRI DS    CL(L'COMMAND)                                                    
DMGR_DEL DS    CL(L'COMMAND)                                                    
*                                                                               
DMGETREC DS    CL(L'COMMAND)                                                    
DMPUTREC DS    CL(L'COMMAND)                                                    
DMADDREC DS    CL(L'COMMAND)                                                    
*                                                                               
WKELEMSW DS    X                   SWITCH TO INDICATE ELEMS EDITED              
SUBMDELQ EQU   X'80'               NEED TO ADD PUBPYELQ ELEM                    
*                                                                               
SVSUBMED DS    CL(L'PUBPYSMD)                                                   
SVPAYNCD DS    CL(L'PUBPYNCD)                                                   
SVPAY_CD DS    CL(L'PUBPY_CD)                                                   
*                                                                               
SVSMDESP DS    CL(L'PSMDDESC)                                                   
*                                                                               
LTLIND   DS    CL1                                                              
ASWITCH  DS    CL1                                                              
ESWITCH  DS    CL1                                                              
ELCODE   DS    X                                                                
*                                                                               
DMWORK1  DS    12D                                                              
SAVERE   DS    F                                                                
SAVEKEY  DS    XL(L'KEY)                                                        
WKSVAREC DS    XL(L'AREC)                                                       
WKERRSW  DS    X                                                                
*                                                                               
WKBYTE1  DS    X                                                                
WKBYTE2  DS    X                                                                
WKHALF1  DS    H                                                                
WKHALF2  DS    H                                                                
WKFULL1  DS    F                                                                
WKFULL2  DS    F                                                                
WKDUB1   DS    D                                                                
WKDUB2   DS    D                                                                
*                                                                               
SPUBCVEN DS    CL12         SAVE FOR POSSIBLE DELETE                            
ELEAREA  DS    500C                                                             
*                                                                               
PUBIO2   DS    4000C                                                            
*                                                                               
WKAIO1   DS    XL4096                                                           
*                                                                               
PPPUB0AX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MISSERR  EQU   001                                                              
FLDINVER EQU   002                                                              
NOTNUMER EQU   003                                                              
PUBERR   EQU   018                                                              
DATERR   EQU   020                                                              
TOOLNGER EQU   032                                                              
CLTAUTH  EQU   033                                                              
RECNFDER EQU   053                                                              
*                                                                               
COMBERR  EQU   112                                                              
REPERR   EQU   122                                                              
INCMPERR EQU   179                                                              
ADJERR   EQU   180                                                              
RECMAXER EQU   204                                                              
MAXEXERR EQU   325                                                              
*                                                                               
B$ADD    EQU   1    BACT = ADD                                                  
B$CHA    EQU   2           CHANGE                                               
B$STN    EQU   3           STND                                                 
B$DIS    EQU   4           DISPLAY                                              
B$COP    EQU   5           COPY                                                 
B$LIS    EQU   6           LIST                                                 
B$FORM   EQU   1    FORMAT                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PPPUBWRK                                                       
*                                                                               
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBFAD                                                       
*                                                                               
LTLREPD  DSECT                                                                  
       ++INCLUDE PUBREPEL                                                       
*                                                                               
LTLPAYD  DSECT                                                                  
       ++INCLUDE PPGENPUBPY                                                     
*                                                                               
       ++INCLUDE PPGENSMED                                                      
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE PPSRCHPARM        PPSRCHPARM                                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPPUB0A   01/03/05'                                      
         END                                                                    
