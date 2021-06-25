*          DATA SET PPPUB0AS   AT LEVEL 058 AS OF 05/01/02                      
*PHASE T4060AA,+0,NOAUTO                                                        
*INCLUDE PUBVAL                                                                 
*INCLUDE PUBEDIT                                                                
*INCLUDE SRCHCALL                                                               
         TITLE 'T4060A PROGRAM UPDATE LOG'                                      
*                                                                               
* BPLA  10/97   RESTORE CHECKS FOR SJR, HD, AND DM                              
*               SINCE IDIOTS NEVER RELEASED UPDATE                              
*               AND I NEED TO GET STEREO EMULATOR READY                         
*               NOTE - PPPUB0AP AND PPPUBFAP HAVE                               
*               ALL THE PUB-PAY CONTROLS                                        
*               TO INSTALL - RENAME PPPUBFAP TO PPPUBFA                         
*               AND PANGEN THE NEW SCREEN                                       
*                                                                               
* BPLA  5/96    REMOVE CHECKS FOR SJR HD AND DM                                 
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*                                                                               
* BPLA 4/5/95  ADD PAY ONLY IF CASH RECEIVED FIELD                              
*              ONLY DISPLAY FOR SJR, HDTO AND DOREMUS FOR NOW                   
*                                                                               
* BPLA 10/2/91 UPDATE FOR REP NAME SEARCHING AND TO USE PPPUBWRK                
*                                                                               
*ROSA  11/9/88 CLEAR REP AND VENDOR DESCRIPTIONS                                
*                                                                               
* ROSA 10/4/88 ADD NEW FIELDS FOR PRINTING STANDARD COMMENTS 1+2                
*              TO INSERTION ORDER PGMS                                          
*                                                                               
         TITLE 'T4060A PUBFILE CLIENT REP OVERRIDE'                             
         PRINT NOGEN                                                            
T4060A   CSECT                                                                  
         NMOD1 0,T4060A,R8,RR=R9                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         LA    R4,ELEAREA                                                       
         LH    R5,=H'500'                                                       
         BAS   RE,CLEARWRK                                                      
         LA    R4,PUBIO                                                         
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
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
***********                                                                     
         CLI   BACT,B$ADD          ADD                                          
         BNE   SEEIFORM                                                         
         TM    LTLIND,X'30'       CANNOT HAVE AN ELEMENT EXITING W/             
         BZ    SEEIFORM           AN ADD FUNCTION                               
         LA    R3,COMBERR                                                       
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
*********                                                                       
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
         BE    0(R2)        END OF RECORD  RETURN VIA R2                        
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         BR    R6                  NEXT ELEMENT RETURN VIA R6                   
*                                                                               
*                                                                               
CKIND    DS    0H                                                               
         CLI   BYTE2,B$FORM        SEE IF ACTION =FORMAT                        
         BE    FORMATP                                                          
*                                  NEW CIRC SCREEN IN TWA SO EDIT IT            
*                                UNLESS ACTION= SRDS OR DISPLAY                 
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
*                                  CLIENT SCREEN IN TWA SO EDIT IT              
*                                  UNLESS ACTION=DISPLAY                        
EDIT     B     FINDREL                                                          
         USING LTLREPD,R4                                                       
EDITR    XC    PUBPAREP,PUBPAREP                                                
         XC    CLIPRN,CLIPRN                                                    
         FOUT  CLIPRNH                                                          
         MVI   ESWITCH,0                                                        
         CLI   CLIPREPH+5,0                                                     
         BE    CKTREP                                                           
         LA    R2,CLIPREPH                                                      
         BAS   RE,EDTREP                                                        
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
         BAS   RE,EDTREP                                                        
         MVI   ESWITCH,1                                                        
         MVC   PUBTRREP,CLITREP                                                 
         FOUT  CLITREPH                                                         
         FOUT  CLITRNH,PREPNAME,30                                              
*                                                                               
*                                                                               
CKCREP   XC    PUBCNREP,PUBCNREP                                                
         XC    CLICRN,CLICRN                                                    
         FOUT  CLICRNH                                                          
         CLI   CLICREPH+5,0                                                     
         BE    CKVEN                                                            
         LA    R2,CLICREPH                                                      
         BAS   RE,EDTREP                                                        
         MVI   ESWITCH,1                                                        
         MVC   PUBCNREP,CLICREP                                                 
         FOUT  CLICREPH                                                         
         FOUT  CLICRNH,PREPNAME,30                                              
*                                                                               
CKVEN    DS    0H                                                               
         XC    CLIPUBN,CLIPUBN                                                  
         FOUT  CLIPUBNH                                                         
         LA    R2,CLIDESH                                                       
         CLC   BAOFR,=C'  '        NO AGENCY OF RECORD                          
         BNH   CKVEN20                                                          
         TM    BACTL,X'80'         PUB TRANSLATION REQUIRED                     
         BZ    CKVEN20             NO                                           
*                                                                               
*        INPUT MUST BE A PUB ON AGY OF REC FILE                                 
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,1          MISSING FIELD                                      
         B     ERROR                                                            
*                                                                               
*                                                                               
*              NOTE USE OF WORK                                                 
*                                                                               
         GOTO1 =V(PUBVAL),DMCB,(5(R2),CLIDES),(0,WORK),RR=RELO                  
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
         BAS   RE,READPUB                                                       
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BAS   RE,GETPUB                                                        
         L     R9,SAVERE           RESTORE R9                                   
*                                                                               
*                                                                               
         FOUT  CLIPUBNH,PUBIO2+35,20                                            
*              NOW CHECK FOR SPECIAL CLIENT ELEMENTS                            
*              ERROR IF I FIND ANY                                              
*              EXCEPT FOR THIS PUB                                              
*                                                                               
*                                  MAKE SURE PUB HAS A BCLT CLT/VEN             
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
         CLC   17(6,R7),PUBIO+1      SAME AS THIS PUB - OK                      
         BE    CKVEN6X                                                          
         LA    R3,PUBERR                                                        
         B     ERROR               AGY OF RECORD  PUB  HAS AN ASSGN             
*                                                                               
CKVEN4   SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     CKVEN3                                                           
*                                                                               
*              NO ASSGN FOUND NEED TO ADD ONE                                   
*                                                                               
CKVEN6   XC    ELEAREA+100(50),ELEAREA+100                                      
         LA    R6,ELEAREA+100                                                   
         USING LTLREPD,R6                                                       
         MVC   0(2,R6),=X'142D'          NEW LENGTH OF 45                       
         MVC   2(1,R6),=X'FF'                                                   
         MVC   3(2,R6),AGYALPHA                                                 
         MVC   PUBCVEN(6),PUBIO+1  PUB NUMBER                                   
*              ADD ELEMENT                                                      
         GOTO1 VRECUP,DMCB,(1,PUBIO2),LTLREPD,0(R7)                             
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BAS   RE,PUTPUB                                                        
         L     R9,SAVERE           RESTO                                        
*                                                                               
*              SEE IF IT WAS ASSGNED TO ANOTHER PUB                             
*              IF SO NEED TO DELETE ELEM                                        
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING LTLREPD,R4                                                       
*                                                                               
CKVEN6X  OC    SPUBCVEN,SPUBCVEN                                                
         BZ    CKVEN8              DONE                                         
         CLI   SPUBCVEN,X'99'      SEE IF IT WAS A PUB NUMBER                   
         BH    CKVEN8              NO - DONE                                    
*                                                                               
         CLC   SPUBCVEN(6),WORK        NO CHANGE IN ASSGNS                      
         BE    CKVEN8                  SO DON'T DELETE ELEM                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BMED                                                      
         MVC   KEY+1(6),SPUBCVEN                                                
         MVC   KEY+7(2),BAOFR      AGY OF REC                                   
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(10),KEYSAVE                                                  
         BNE   CKVEN8              NOT FOUND  - DONE                            
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BAS   RE,GETPUB                                                        
         L     R9,SAVERE                                                        
*                                                                               
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
*                                                                               
CKVEN7A  SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     CKVEN7                                                           
*                                                                               
CKVEN7B  GOTO1 VRECUP,DMCB,(1,PUBIO2),0(R7),0        DELETE ELEMENT             
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BAS   RE,PUTPUB                                                        
         L     R9,SAVERE                                                        
*                                                                               
*                                                                               
CKVEN8   MVC   KEY,SAVEKEY         RESTORE KEY AND DMWORK                       
         MVC   DMWORK(96),DMWORK1                                               
         XC    PUBCVEN,PUBCVEN                                                  
         MVC   PUBCVEN(6),WORK         WORK HAS NEW ASSGN                       
         B     CKVEN20E                                                         
*                                                                               
CKVEN20  DS    0H                                                               
         CLI   5(R2),12            MAX 12 CHARS                                 
         BNH   CKVEN20A                                                         
CKVENER1 LA    R3,FLDINV                                                        
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
CKVEN20X DS    0H                                                               
*                                                                               
*  AT THIS POINT ADD CODE TO EDIT THE NEW FIELDS                                
*                                                                               
         FOUT   CLICOM1H                                                        
         CLI    CLICOM1H+5,0                                                    
         BE     EDWHR1            EDIT WHERE COMMENTS ARE TO GO                 
         LA     R2,CLICOM1H                                                     
**                                                                              
         CLC    BCLT,=3X'FF'      DON'T ALLOW FOR ALL CLIENTS                   
         BE     ALLCERR                                                         
         BAS    RE,CHKOUT         SEE IF COMMENT EXITST                         
         BE     *+12                                                            
         LA     R3,CMNTERR                                                      
         B      ERROR                                                           
* RIGHT JUSTIFY                                                                 
         MVC    WORK+10(20),SPACES                                              
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
         SH     RF,=H'5'                                                        
         MVC    PUBCSC1,0(RF)                                                   
         MVI    ESWITCH,1                                                       
*                                                                               
*                                                                               
EDWHR1   FOUT   CLIWHR1H                                                        
         CLI    CLIWHR1H+5,0                                                    
         BNE    WHR1EDT                                                         
         CLI    CLICOM1H+5,0                                                    
         BE     CHKNEXT                                                         
         LA     R2,CLIWHR1H                                                     
         B      MISSING                                                         
MISSING1 LA     R2,CLICOM1H                                                     
MISSING  LA     R3,1                                                            
         B      ERROR                                                           
*                                                                               
WHR1EDT  CLI    CLICOM1H+5,0         IF STD COMMENT 1 IS 0-ERROR                
         BE     MISSING1                                                        
*                                                                               
         CLI    CLIWHR1,C'I'         INSERTION ORDERS FOR NOW                   
         MVI    PUBCSCC1,X'80'                                                  
         BE     CHKNEXT                                                         
         LA     R2,CLIWHR1H                                                     
         LA     R3,2                 INVALID INPUT                              
         B      ERROR                                                           
*                                                                               
CHKNEXT  DS    0H                                                               
         FOUT   CLICOM2H                                                        
         CLI    CLICOM2H+5,0                                                    
         BE     EDWHR2            EDIT WHERE COMMENTS ARE TO GO                 
         LA     R2,CLICOM2H                                                     
         CLC    BCLT,=3X'FF'      DON'T ALLOW FOR ALL CLIENTS                   
         BE     ALLCERR                                                         
         CLI    CLICOM1H+5,0      NOTHING MUST BE IN FIRST SET                  
         LA     R2,CLICOM1H       WHERE CURSOR IS TO BE POSITIONED              
         BE     MISSING           EDIT WHERE COMMENTS ARE TO GO                 
         LA     R2,CLICOM2H                                                     
* RIGHT JUSTIFY                                                                 
         MVC    WORK+10(20),SPACES                                              
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
         SH     RF,=H'5'                                                        
         MVC    PUBCSC2,0(RF)                                                   
         MVI    ESWITCH,1                                                       
*                                                                               
         BAS    RE,CHKOUT         SEE IF COMMENT EXITST                         
         BE     *+12                                                            
         LA     R3,CMNTERR                                                      
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
WHR2EDT  CLI    CLICOM2H+5,0         IF STD COMMENT 2 IS 0-ERROR                
         LA     R2,CLICOM2H                                                     
         BE     MISSING                                                         
*                                                                               
         MVI    PUBCSCC2,X'80'                                                  
         LA     R2,CLIWHR2H                                                     
         CLI    CLIWHR2,C'I'         INSERTION ORDERS FOR NOW                   
         BE     NONEXT                                                          
         LA     R3,2                 INVALID INPUT                              
         B      ERROR                                                           
*                                                                               
NONEXT   DS    0H                                                               
*                                                                               
POCR     DS    0H                   PAY ONLY IF CASH RECEIVED                   
         MVI   PUBCCTL,0                                                        
         LA    R2,CLIPOCRH                                                      
         CLI   5(R2),0                                                          
         BE    POCRX                                                            
*                              ACCEPT N OR Y; NO OR YES                         
         CLI   5(R2),1                                                          
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
         LA     R3,2                 INVALID INPUT                              
         B      ERROR                                                           
*                                                                               
*                                                                               
POCR5    DS    0H              MORE THAN 1 CHARACTER INPUT                      
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
         B     UPDATER                                                          
*                                                                               
ALLCERR  LA    R3,INVERR        NO STND COMMENTS FOR 'ALL' CLIENT               
         B     ERROR            SCREEN                                          
*                                                                               
*                                                                               
FINDREL  LA    R4,PUBREC+33                                                     
         CLI   0(R4),0                                                          
         BNE   CKREL1                                                           
         MVI   ASWITCH,1                                                        
         B     EDITR                                                            
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
         LA    R4,ELEAREA             BUILD REP ELEM IN ELEM AREA               
         B     EDITR                                                            
*                        ASWITCH=1 IF PUBREPEL DID NOT EXIST                    
*                        RECUP TO ADD IT IF ESWITCH=1 OR DELETE IT              
*                        IF ESWITCH=0                                           
*                                                                               
UPDATER  DS    0H                                                               
*        CLI   ESWITCH,0                                                        
*        BE    DELETER                                                          
*        CLI   ASWITCH,1                                                        
*        BNE   EDITX                                                            
*        B     UPDATE2                                                          
*                                                                               
UPDATE1  CLI  ESWITCH,0                                                         
         BE    EDITX                                                            
UPDATE2  MVC   PUBREPEL(2),=X'142D'                                             
         MVC   PUBRPOFF(3),BCLT                                                 
         LA    R5,PUBREC+33  SET R5 TO WHERE  I WANT TO ADD ELEM                
         CR    R5,R4              WILL BE EQUAL IF REC HAD NO ELEMS             
         BE    UPDATE4        GO ADD ELEM                                       
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
*                                                                               
WRITEIT  OC    PUBADDR,PUBADDR                                                  
         BZ    ADDIT                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
*                                                                               
ADDIT    DS    0H                                                               
         B     DONE                                                             
*                                                                               
CHKOUT   DS    0H           VERIFY EXISTANCE OF STD COMMENT                     
         SPACE 3                                                                
         NTR1                                                                   
*          DATA SET PPLFM04    AT LEVEL 022 AS OF 06/10/88                      
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
ED15     MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),SPACES                                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    ED16         NO INPUT // NO CHECKING                             
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1                                                            
         AH    RF,=H'6'                                                         
         LA    RF,KEY+4(RF)                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)       MOVE TO KEY+4+(6-L)                          
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ED16                                                             
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,HIGH         RESTORE D/M POINTERS                             
*                                                                               
*                                                                               
         CR    RE,RB        ERROR PATH  FORCE BNE                               
         B     XXIT                                                             
*                                                                               
ED16     DS    0H            NON-ERROR PATH // FORE BE                          
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,HIGH         RESTORE D/M POINTERS                             
         CR    RE,RE                                                            
XXIT     XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
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
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'0A'                                                    
*                                                                               
         CLC   AGYALPHA,=C'SJ'       SJR                                        
         BE    FMT5                                                             
         CLC   AGYALPHA,=C'HD'       HDTO                                       
         BE    FMT5                                                             
         CLC   AGYALPHA,=C'DM'       DOREMUS                                    
         BE    FMT5                                                             
*                                 IF NOT ONE OF THESE AGYS                      
*                                 CLEAR 'PAY ONLY IF CASH RECEIVED'             
         XC    CLIPOCA,CLIPOCA                                                  
         XC    CLIPOCR,CLIPOCR                                                  
         FOUT  CLIPOCRH                                                         
         OI    CLIPOCRH+1,X'20'    AND PROTECT                                  
*                                                                               
FMT5     FOUT  CLIPREPH,SPACES,4                                                
         FOUT  CLITREPH,SPACES,4                                                
         FOUT  CLICREPH,SPACES,4                                                
         FOUT  CLIDESH,SPACES,15                                                
         FOUT  CLICOM1H,SPACES,6                                                
         FOUT  CLICOM2H,SPACES,6                                                
         FOUT  CLIWHR1H,SPACES,1                                                
         FOUT  CLIWHR2H,SPACES,1                                                
         FOUT  CLIPRNH,SPACES,30             NAMES                              
         FOUT  CLITRNH,SPACES,30                                                
         FOUT  CLICRNH,SPACES,30                                                
         FOUT  CLIPUBNH,SPACES,30                                               
         FOUT  CLIPOCRH,SPACES,4                                                
*                                                                               
         TM    LTLIND,X'30'                                                     
         BNZ   PUTFLDS                                                          
         LA    R2,CLIPREPH                                                      
         B     EXIT                                                             
*                                                                               
PUTFLDS  TM    LTLIND,X'10'  NO ELEMENT FOUND                                   
         BNO   PUTVENXX      WAS PUBVEN                                         
         LA    R4,PUBREC+33                                                     
PUTFLD1  CLI   0(R4),X'14'                                                      
         BNE   NEXT1                                                            
         CLC   2(3,R4),BCLT                                                     
         BNE   NEXT1                                                            
         USING LTLREPD,R4                                                       
         LA    R2,CLIPREPH                                                      
         LA    R5,CLIPRNH                                                       
         FOUT  CLIPRNH,SPACES,30             NAMES                              
         FOUT  CLITRNH,SPACES,30                                                
         FOUT  CLICRNH,SPACES,30                                                
         FOUT  CLIPUBNH,SPACES,30                                               
         OC    PUBPAREP,PUBPAREP                                                
         BZ    PUTREP1                                                          
         FOUT  CLIPREPH,PUBPAREP,4                                              
         BAS   RE,PUTRNAME                                                      
PUTREP1  LA    R2,CLITREPH                                                      
         LA    R5,CLITRNH                                                       
         OC    PUBTRREP,PUBTRREP                                                
         BZ    PUTREP2                                                          
         FOUT  CLITREPH,PUBTRREP,4                                              
         BAS   RE,PUTRNAME                                                      
PUTREP2  LA    R2,CLICREPH                                                      
         LA    R5,CLICRNH                                                       
         OC    PUBCNREP,PUBCNREP                                                
         BZ    PUTVEN                                                           
         FOUT  CLICREPH,PUBCNREP,4                                              
         BAS   RE,PUTRNAME                                                      
         B     PUTVEN                                                           
*                                                                               
PUTVEN   DS    0H                                                               
*                     DETERMINE IF OLD ELEMENT -- IF SO DO NOT                  
*                     DISPLAY                                                   
         CLI   1(R4),32         OLD LENGTH                                      
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
*        ONLY DISPLAY 'PAY ONLY IF CASH RECEIVED'                               
*        FOR THESE AGENCIES                                                     
*        (FOR NOW - SINCE UPDATE STILL NOT RELEASED)                            
*                                                                               
         CLC   AGYALPHA,=C'SJ'                                                  
         BE    PUTPAY                                                           
         CLC   AGYALPHA,=C'HD'                                                  
         BE    PUTPAY                                                           
         CLC   AGYALPHA,=C'DM'                                                  
         BE    PUTPAY                                                           
         B     PUTVENXX                                                         
*                                                                               
PUTPAY   DS    0H                                                               
         MVC   CLIPOCR(2),=C'NO'                                                
         TM    PUBCCTL,X'01'                                                    
         BNO   *+10                                                             
         MVC   CLIPOCR(3),=C'YES'                                               
         FOUT  CLIPOCRH                                                         
*                                                                               
PUTVENXX DS    0H                                                               
         XC    CLIPUBN,CLIPUBN                                                  
         XC    CLIDES,CLIDES                                                    
         TM    BACTL,X'80'                                                      
         BZ    PUTVEN4                                                          
*                                                                               
*              SHOULD BE PUB NUMBER TRY TO DISPLAY                              
*              AND READ PUB TO DISPLAY NAME                                     
*                                                                               
         MVC   SAVEKEY(32),KEY                                                  
         MVC   DMWORK1(96),DMWORK                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BMED                                                      
         MVC   KEY+1(6),PUBCVEN                                                 
         MVC   KEY+7(2),BAOFR      AGY OF REC                                   
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(10),KEY                                                  
         BE    PUTVEN1                                                          
         FOUT  CLIPUBNH,=C'** AGY OF REC PUB NOT FOUND **',30                   
         CLI   PUBCVEN,X'99'          NOT A PUB NUMBER                          
         BH    PUTVEN4                                                          
         B     PUTVEN2                                                          
*                                                                               
*                                                                               
PUTVEN1  ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BAS   RE,GETPUB                                                        
         L     R9,SAVERE                                                        
*                                                                               
         FOUT  CLIPUBNH,PUBIO2+35,20                                            
*                                                                               
PUTVEN2  DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         MVC   DMWORK(96),DMWORK1                                               
         IC    R5,APROF13                                                       
         GOTO1 =V(PUBEDIT),DMCB,((R5),PUBCVEN),(0,CLIDES),RR=RELO               
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
         BH    DONE                                                             
         LA    R2,CLIPREPH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
PUTRNAME ST    RE,SAVERE                                                        
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY                                                   
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),8(R2)                                                   
         BAS   RE,HIGH                                                          
         MVC   DMWORK1(96),DMWORK                                               
         CLC   KEYSAVE(25),KEY                                                  
         BNE   NOREP                                                            
         BAS   RE,GETREC                                                        
         FOUT  (R5),PREPNAME,30                                                 
         B     RETURN                                                           
*                                                                               
NOREP    XC    8(30,R5),8(R5)                                                   
         MVC   8(19,R5),=C'* REP NOT ON FILE *'                                 
RETURN   MVC   KEY(32),SAVEKEY                                                  
         MVC   DMWORK(96),DMWORK1                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
LTLIND   DS    CL1                                                              
ELCOD    DS    CL1                                                              
ASWITCH  DS    CL1                                                              
ESWITCH  DS    CL1                                                              
FLDINV   EQU   2                                                                
*          DATA SET PPLFM04    AT LEVEL 022 AS OF 06/10/88                      
*                                  ERROR EQUATES                                
MISSERR  EQU   1                                                                
INVERR   EQU   2                                                                
PUBERR   EQU   18                                                               
DATERR   EQU   20                                                               
CLTAUTH  EQU   33                                                               
CMNTERR  EQU   53                                                               
*                                                                               
COMBERR  EQU   112                                                              
REPERR   EQU   122                                                              
INCMPERR EQU   179                                                              
ADJERR   EQU   180                                                              
*                                                                               
ELCNT    DC    H'0'                                                             
VIRGERR  DC    H'0'                                                             
SPACES   DC    40C' '                                                           
DMWORK1  DS    12D                                                              
SAVERE   DS    F                                                                
SAVEKEY  DS    CL32                                                             
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
NEXT1    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    PUTVENXX                                                         
         B     PUTFLD1                                                          
         SPACE 3                                                                
EDTREP   DS    0H                                                               
         ST    RE,SAVERE                                                        
******                                                                          
******   REP NAME SEARCHING                                                     
******                                                                          
         ST    R2,FULL                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,BMED                                                    
         DROP  R3                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'REP'),0,RR=RELO                         
         L     R2,FULL        RESTORE R2                                        
*                                                                               
         LA    R3,REPERR                                                        
         CLI   8(R2),C'A'                                                       
         BNE   EDTREP2                                                          
         CLI   5(R2),4                                                          
         BNE   ERROR                                                            
         MVC   KEY+4(4),8(R2)                                                   
         B     EDTREP4                                                          
*                                                                               
EDTREP2  DS    0H                                                               
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+4(4),DUB+5(3)                                                
EDTREP4  MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'11'                                                      
         BAS   RE,READ                                                          
         MVC   DMWORK1(96),DMWORK                                               
         BAS   RE,GETREC                                                        
         MVC   DMWORK(96),DMWORK1                                               
         MVC   8(4,R2),KEY+4                                                    
         L     RE,SAVERE                                                        
         MVC   KEY(32),SAVEKEY                                                  
         BR    RE                                                               
*                                                                               
NXTUNP   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NXTUNP                                                           
         BR    RE                                                               
*                                                                               
*                                                                               
WRTREC   SR    R5,R5                                                            
         IC    R5,PUBIO+25                                                      
         SLL   R5,8                                                             
         IC    R5,PUBIO+26                                                      
         SR    RE,RE                                                            
         LA    RE,PUBIO                                                         
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PUBIO+1999                                                    
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
*                                                                               
B$ADD    EQU   1    BACT = ADD                                                  
B$CHA    EQU   2           CHANGE                                               
B$STN    EQU   3           STND                                                 
B$DIS    EQU   4           DISPLAY                                              
B$COP    EQU   5           COPY                                                 
B$LIS    EQU   6           LIST                                                 
B$FORM   EQU   1    FORMAT                                                      
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE PUGENEROL                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
SPUBCVEN DS    CL12         SAVE FOR POSSIBLE DELETE                            
ELEAREA  DS    500C                                                             
*                                                                               
PUBIO2   DS    4000C                                                            
*                                                                               
       ++INCLUDE PPPUBWRK                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBFAD                                                       
*                                                                               
LTLREPD  DSECT                                                                  
       ++INCLUDE PUBREPEL                                                       
*                                                                               
*  PPSRCHPARM                                                                   
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058PPPUB0AS  05/01/02'                                      
         END                                                                    
