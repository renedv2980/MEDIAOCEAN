*          DATA SET TAGENF2    AT LEVEL 005 AS OF 12/30/13                      
*PHASE T702F2E,*                                                                
         TITLE 'T702F2 - EVTIME2 LIST'                                          
T702F2   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T702F2,R7,R6                                              
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=LOCAL WORKING STORAGE                     
         USING MYD,R7                                                           
                                                                                
         OI    GENSTAT1,RDUPAPPL                                                
                                                                                
         LA    RE,LINTAB                                                        
         ST    RE,ALINTAB                                                       
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         JNE   *+8                                                              
         BRAS  RE,LR                                                            
                                                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERNFD    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
ERIPD    MVI   ERROR,ERINVPD       INVOICE IS ALREADY PAID                      
         J     END                                                              
                                                                                
EREMYNF  LA    R2,ET2GTEH                                                       
         LHI   RE,ERREMYNF         GO TO EMPLOYEE NOT FOUND                     
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERNEG    L     R2,ALSTPOAR         TOTAL CANNOT BE NEGATIVE                     
         LHI   RE,EREVTNEG                                                      
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERAEXTAX LHI   RE,ERREVTET                                                      
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERTEXTAX LHI   RE,ERREVTTT                                                      
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERAEX7TX LHI   RE,ERREVTE7                                                      
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERTEX7TX LHI   RE,ERREVTT7                                                      
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
CONFIRM  LA    R2,ET2L1H                                                        
         LHI   RE,276              CONFIRM ALL CHANGES COMPLETE                 
         STH   RE,MYMSGNO                                                       
         J     INFEND                                                           
                                                                                
ERRW4PC  LHI   RE,ERRW4PRV         W4 MISSING PROVINCE CODE                     
         STH   RE,MYMSGNO                                                       
         MVI   BLOCK,L'SVPID+1                                                  
         MVC   BLOCK+1(L'SVPID),SVPID     DISPLAY PID                           
         MVI   BLOCK+1+L'SVPID,0                                                
         OI    GENSTAT2,USGETTXT                                                
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
INFEND   MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
ERRMORE  LA    R2,ET2L1TH                                                       
         LHI   RE,9                                                             
         STH   RE,MYMSGNO                                                       
         MVI   MYMSYS,X'FF'                                                     
         J     INFEND                                                           
                                                                                
ERREOL   LA    R2,ET2L1TH                                                       
         LHI   RE,10                                                            
         STH   RE,MYMSGNO                                                       
         MVI   MYMSYS,X'FF'                                                     
         J     INFEND                                                           
                                                                                
END      GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 INITIAL,DMCB,PFTAB                                               
                                                                                
         MVC   TIABUFF,AIO3                                                     
         LA    RF,2000                                                          
         ST    RF,TILBUFF                                                       
                                                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINES                     *         
***********************************************************************         
                                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF24X-*,24,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF24X    DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   ACTNUM,ACTLIST                                                   
         JNE   XIT                                                              
                                                                                
         GOTO1 FLDVAL,DMCB,(X'40',ET2AGYH),(X'80',ET2GTEH)                      
         JE    XIT                                                              
         GOTO1 FLDVAL,DMCB,(X'01',ET2L1H),(X'00',ET2ENDH)                       
         XC    SVCAKEY,SVCAKEY     KEY CHANGED, START ANEW                      
         MVI   PROSTAT,0                                                        
                                                                                
         XC    FRSTCAKY,FRSTCAKY                                                
         OI    PROSTAT,PSGOTORE                                                 
                                                                                
         CLI   ET2GTEH+5,0                                                      
         BE    *+8                                                              
         NI    PROSTAT,X'FF'-PSGOTORE                                           
                                                                                
         MVC   ENTRIES(LINTBLNQ),LINTAB                                         
                                                                                
         L     R4,AIO                                                           
         GOTO1 VKAGY,DMCB,ET2AGYH                                               
         MVC   TIFAGY,TGAGY                                                     
                                                                                
         GOTO1 VKINV,DMCB,ET2INVH,ET2EVTH,ET2PERH                               
                                                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
                                                                                
         GOTO1 FLDVAL,DMCB,(X'22',ET2AGYH),ET2GTEH                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE AGENCY                                   *         
*        ON ENTRY ... P1=A(AGENCY FIELD)                                        
*                     R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKAGY    NTR1                                                                   
         L     R2,0(R1)                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'22',(R2))                                 
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL            ENSURE AGENCY IS PRODUCTIONS                 
         JE    *+6                 PLUS AGENCY                                  
         DC    H'00'                                                            
         TM    TAAYSTA7,TAAYSPPL                                                
         JZ    ERINV                                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE INVOICE                                  *         
*        ON ENTRY ... P1=A(INVOICE FIELD)                             *         
*                     P2=A(EVENT FIELD)                               *         
*                     P3=A(PAY PERIOD FIELD)                          *         
*                     R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKINV    NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R5,8(R1)                                                         
                                                                                
         CLI   5(R2),0                                                          
         JNE   VK10                                                             
         OC    TGINV,TGINV                                                      
         JZ    ERMIS                                                            
         CLI   TGINV+5,X'FF'                                                    
         JNE   *+10                                                             
         XC    TGINV,VKHEXFFS                                                   
         GOTO1 TINVCON,DMCB,TGINV,8(R2),DATCON                                  
         MVI   5(R2),6                                                          
                                                                                
VK10     GOTO1 TINVCON,DMCB,8(R2),TGINV,DATCON                                  
         CLI   0(R1),X'FF'                                                      
         JE    ERINV                                                            
         XC    TGINV,=6X'FF'                                                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',0)                                    
         JNE   ERNFD                                                            
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAINTMCO,TAINTMCO                                                
         JZ    ERINV                                                            
         GOTO1 DATCON,DMCB,(X'11',TAINPTPD),(8,8(R5))                           
         MVC   TGPCYC,TAINPTPD                                                  
                                                                                
         NI    PROSTAT,X'FF'-PSINVPD-PSCHKCUT                                   
         OC    TAINPDTE,TAINPDTE   INVOICE PAID?                                
         JZ    *+8                                                              
         OI    PROSTAT,PSINVPD                                                  
         OC    TAINCDTE,TAINCDTE   CHECKS CUT?                                  
         JZ    *+8                                                              
         OI    PROSTAT,PSCHKCUT                                                 
                                                                                
VK20     GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',TAINTMCO)                            
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGCLI,TLCOCLI                                                    
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   8(L'TACOCID,R3),TACOCID                                          
         MVC   TGCID,TACOCID                                                    
                                                                                
         DROP  R4                                                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
VKHEXFFS DC    10X'FF'                                                          
                                                                                
LINTAB   DS    0F                  DISPLACEMENT TO LIST LINES 1-8               
         DC    AL2(ET2L1H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(ET2L2H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(ET2L3H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(ET2L4H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(ET2L5H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(ET2L6H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(ET2L7H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(ET2L8H-T702FFD),CL2' ',AL4(0,0),AL1(0)                       
         DC    AL2(0)                                                           
LINTBLNQ EQU   *-LINTAB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO FIND CURRENT ITEM'S LINE, DISKADD AND TAXABLE     *         
***********************************************************************         
                                                                                
FINDITEM NTR1  BASE=*,LABEL=*                                                   
                                                                                
         SR    RF,RF                                                            
         IC    RF,CURRITEM         CURRENT ITEM                                 
         MH    RF,=Y(LISTLNQ)                                                   
                                                                                
         USING LISTD,RE                                                         
         LA    RE,ENTRIES                                                       
         AR    RE,RF                                                            
         ST    RE,CURRLIST                                                      
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,LISTLDSP       DISPLACEMENT                                 
         LA    R2,T702FFD                                                       
         AR    R2,R1                                                            
         ST    R2,CURRLIN                                                       
                                                                                
         MVC   W4PROV,LISTW4PR                                                  
         MVC   TSHTDSKA,LISTLDA                                                 
         MVC   TXBLAMNT,LISTTXBL                                                
         MVC   W4STAT,LISTW4ST                                                  
                                                                                
         BRAS  RE,TXBL70           GET 70% OF TAXABLE AMOUNT                    
         J     XIT                                                              
                                                                                
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO LIST RECORDS                                      *         
***********************************************************************         
                                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         OC    FRSTCAKY,FRSTCAKY   FIRST TIME IN?                               
         JNZ   LR020                                                            
                                                                                
         OI    PROSTAT,PSGOTORE                                                 
         CLI   ET2GTEH+5,0                                                      
         BE    *+8                                                              
         NI    PROSTAT,X'FF'-PSGOTORE                                           
                                                                                
LR020    BRAS  RE,VALLSTLN         VALIDATE LIST LINES, IF NEEDED               
                                                                                
LR030    MVI   CURRITEM,0                                                       
         GOTO1 FLDVAL,DMCB,(X'01',ET2L1H),(X'00',ET2ENDH)                       
                                                                                
         LA    R5,KEY                                                           
         OC    SVCAKEY,SVCAKEY     NEXT PAGE OF CAST?                           
         JZ    LR050                                                            
         MVC   KEY,SVCAKEY         YES                                          
         J     LR080                                                            
                                                                                
         USING TLCAD,R5                                                         
LR050    XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ      READ CAST BASED ON CAST SEQ                  
         MVC   TLCACOM,TGCOM                                                    
LR080    GOTO1 HIGH                                                             
         J     LR110                                                            
                                                                                
LR100    GOTO1 SEQ                                                              
LR110    CLC   KEY(TLCASRT-TLCAD),KEYSAVE      SAME CAST COMML?                 
         JNE   LR900                                                            
                                                                                
         CLI   CURRITEM,0                                                       
         BNE   *+10                                                             
         MVC   FRSTCAKY,KEY        SAVE FIRST LINE'S CAST KEY                   
                                                                                
         MVC   SVCAKEY,KEY         SAVE KEY TO RESTORE READ SEQ                 
         MVC   SVCASEQ,TLCASEQ       AND CAST INPUT SEQ NUM                     
                                                                                
         CLI   CURRITEM,8          FILLED UP ALL THE LINES                      
         JL    LR200                                                            
         J     ERRMORE                                                          
                                                                                
LR200    MVC   SVSSN,TLCASSN                                                    
                                                                                
         BAS   RE,LRHOOK           HANDLE THIS CAST RECORD                      
         JNE   LR300               NO, GET NEXT ONE                             
                                                                                
         SR    R1,R1               BUMP TO NEXT LINE ITEM                       
         IC    R1,CURRITEM                                                      
         AHI   R1,1                                                             
         STC   R1,CURRITEM                                                      
                                                                                
LR300    MVC   KEY,SVCAKEY         RESTORE CAST READ SEQ                        
         GOTO1 HIGH                                                             
                                                                                
         XC    SVCAKEY,SVCAKEY     GET MORE CAST                                
         J     LR100                                                            
                                                                                
LR900    CLI   ET2GTEH+5,0                                                      
         JE    LR990                                                            
         TM    PROSTAT,PSGOTORE                                                 
         JZ    EREMYNF                                                          
LR990    DS    0H                                                               
         XC    FRSTCAKY,FRSTCAKY                                                
         XC    SVCAKEY,SVCAKEY                                                  
         J     ERREOL                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS RECORDS FROM SYSIO                        *         
***********************************************************************         
LRHOOK   NTR1                                                                   
         USING LINED,R2                                                         
         BRAS  RE,FINDITEM                                                      
         L     R2,CURRLIN                                                       
                                                                                
         GOTO1 SSNPACK,DMCB,SVSSN,SVPID                                         
                                                                                
         MVI   FLDWHDR,L'FLDWHDR                                                
         MVC   FLDWHDR+8(L'FLDWHDR-8),SPACES                                    
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'AC',SVSSN),FLDWHDR                        
         JE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BRAS  RE,W4CAN            CHECK W4 IS CANADIAN                         
         JNE   NO                                                               
*                                                                               
         TM    PROSTAT,PSGOTORE    GOTO EMPLOYEE REACHED?                       
         JO    LRH10               YES, CONTINUE NORMALLY                       
                                                                                
         ZIC   RE,ET2GTEH+5        OTHERWISE CHECK NAME                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   ET2GTE(0),FLDWHDR+8                                              
         JNE   NO                                                               
         OI    PROSTAT,PSGOTORE    GOTO EMPLOYEE REACHED!                       
*                                                                               
         USING TLTMD,R3                                                         
LRH10    LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSPPL                                                 
         MVC   TLTMCOM,TGCOM                                                    
         MVC   TLTMINV,TGINV                                                    
         MVC   TLTMSSN,SVSSN                                                    
         MVC   TLTMSORT+4(L'TLCASEQ),SVCASEQ                                    
         GOTO1 HIGH                                                             
         CLC   TLTMKEY,KEYSAVE                                                  
         JNE   NO                                                               
         DROP  R3                                                               
                                                                                
         USING TLDRD,R3                                                         
LRH20    MVC   TSHTKEY,KEY                                                      
         MVC   TSHTDSKA,TLDRDA                                                  
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         BRAS  RE,GETTXBL          GET TAXABLE AMOUNT                           
                                                                                
         TM    W4STAT,W4STCAN+W4STWKCN   CANADIAN OR WORKED IN CANADA           
         JZ    NO                                                               
                                                                                
LRH25    BRAS  RE,GETTAAT          GET TAAT ELEMENT IF WE HAVE                  
                                                                                
         MVC   LPID,SVPID                                                       
         MVC   LNAME,FLDWHDR+8                                                  
         EDIT  TXBLAMNT,(L'LTAXBL,LTAXBL),2,ZERO=NOBLANK                        
                                                                                
         TM    W4STAT,W4STTAAT                                                  
         JZ    LRH50                                                            
                                                                                
         EDIT  CNTXFD,(L'LFD,LFD),2,ZERO=NOBLANK                                
         EDIT  CNTXPR,(L'LPRQU,LPRQU),2,ZERO=NOBLANK                            
         EDIT  CNTXCPP,(L'LCPPQPP,LCPPQPP),2,ZERO=NOBLANK                       
         EDIT  CNTXEI,(L'LEIQEI,LEIQEI),2,ZERO=NOBLANK                          
                                                                                
         CLC   W4PROV,=C'QC'       QUEBEC                                       
         JNE   LRH30                                                            
         EDIT  CNTXQPIP,(L'LQPIP,LQPIP),2,ZERO=NOBLANK                          
                                                                                
LRH30    GOTO1 FLDVAL,DMCB,(X'22',LPIDH),LNAMEH     VALID & TRANSMIT            
                                                                                
LRH50    TM    W4STAT,W4STERCP                                                  
         JZ    LRH60                                                            
         OI    ET2AGYH+4,X'80'     INPUT THIS TIME                              
         OI    ET2AGYH+6,X'80'                                                  
         LA    R2,LFDH                                                          
         XC    FRSTCAKY,FRSTCAKY                                                
         XC    SVCAKEY,SVCAKEY                                                  
         J     ERRW4PC                                                          
                                                                                
         USING LISTD,RF                                                         
LRH60    L     RF,CURRLIST                                                      
         MVC   LISTW4PR,W4PROV     W4'S PROVINCE                                
         MVC   LISTLDA,TSHTDSKA    SAVE TIMESHEET DISKADD                       
         MVC   LISTTXBL,TXBLAMNT   TAXABLE AMOUNT                               
         MVC   LISTW4ST,W4STAT     AND W4'S STATUS                              
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR LIST RECORD ROUTINES                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE LIST LINES                               *         
***********************************************************************         
VALLSTLN NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   CURRITEM,0                                                       
         MVI   LISTCHGD,C'N'                                                    
                                                                                
VLL100   BRAS  RE,FINDITEM                                                      
                                                                                
         USING LINED,R4                                                         
         L     R4,CURRLIN                                                       
                                                                                
         OC    TSHTDSKA,TSHTDSKA                                                
         BZ    VLL900                                                           
         CLC   LPID,SPACES         ONLY VALIDATE IF THERE IS A PID              
         JNH   VLL900                                                           
                                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         MVC   TLDRDA,TSHTDSKA                                                  
         GOTO1 GETREC                                                           
                                                                                
         LR    R2,R4                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         LR    R4,R2                                                            
         BNE   VLL500                                                           
                                                                                
         GOTO1 FLDVAL,DMCB,(X'40',LPIDH),(X'80',LQPIPH)                         
         JE    VLL900              NO CHANGE ON THIS LINE                       
VLL500   BRAS  RE,LVR                                                           
                                                                                
VLL900   SR    R1,R1                                                            
         IC    R1,CURRITEM         BUMP TO NEXT LINE                            
         AHI   R1,1                                                             
         STC   R1,CURRITEM                                                      
                                                                                
         CLI   CURRITEM,8                                                       
         JL    VLL100                                                           
                                                                                
         CLI   LISTCHGD,C'Y'       LIST WAS CHANGED                             
         JE    XIT                                                              
         XC    FRSTCAKY,FRSTCAKY   NO, CONTINUE WITH NEXT CAST                  
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE LIST RECORDS                             *         
***********************************************************************         
LVR      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING LINED,R4                                                         
         L     R4,CURRLIN                                                       
                                                                                
         XC    CANTAX,CANTAX                                                    
         BAS   RE,VLDCANTX         VALIDATE FD,PR,CPP,EI,QPIP FIELDS            
                                                                                
LVR900   L     RF,CNTXFD           SUM UP ALL THE TAXES                         
         A     RF,CNTXPR                                                        
         A     RF,CNTXCPP                                                       
         A     RF,CNTXEI                                                        
         A     RF,CNTXQPIP                                                      
                                                                                
         TM    W4STAT,W4STWKCN     WORKED IN CANADA?                            
         BO    LVR908                                                           
         C     RF,TXBL7AMT         ALL TAXES CANNOT BE > 70%(TAXABLE)           
         JNH   LVR930                                                           
         LA    R2,LFDH                                                          
         J     ERTEX7TX                                                         
                                                                                
LVR908   C     RF,TXBLAMNT         ALL TAXES CANNOT BE > TAXABLE AMT            
         JNH   LVR930                                                           
         LA    R2,LFDH                                                          
         J     ERTEXTAX                                                         
                                                                                
LVR930   XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         MVC   TLDRDA,TSHTDSKA                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,TAATELQ      REMOVE THE OLD TAAT ELEMENTS                 
         GOTO1 REMELEM                                                          
                                                                                
         USING TAATD,R3                                                         
LVR950   LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAATEL,TAATELQ      ADD CANADIAN ONE                             
         MVI   TAATLEN,TAATLN2Q                                                 
         MVC   TAATUNIT,=C'CN '                                                 
         MVC   TAATTAX,CNTXFD                                                   
         MVC   TAATPP,CNTXCPP      PP/EI/PIP ARE AT FEDERAL LEVEL TOO           
         MVC   TAATEI,CNTXEI                                                    
         MVC   TAATPIP,CNTXQPIP                                                 
         GOTO1 ADDELEM                                                          
                                                                                
         MVC   TAATUNIT,W4PROV     ADD PROVINCIAL ONE                           
         OC    TAATUNIT,SPACES                                                  
         MVC   TAATTAX,CNTXPR                                                   
         MVC   TAATPP,CNTXCPP                                                   
         MVC   TAATEI,CNTXEI                                                    
         MVC   TAATPIP,CNTXQPIP                                                 
         GOTO1 ADDELEM                                                          
                                                                                
         GOTO1 PUTREC                                                           
         MVI   LISTCHGD,C'Y'                                                    
                                                                                
LVR990   DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE CANADIAN TAX FIELDS                      *         
***********************************************************************         
         USING LINED,R4                                                         
VLDCANTX NTR1                                                                   
         LA    R5,5                                                             
         LA    R2,LFDH                                                          
         LA    R3,CNTXFD                                                        
                                                                                
VCANTX50 TM    PROSTAT,PSINVPD     IF INVOICE ALREADY PAID                      
         JO    ERIPD               ERROR, CAN'T CHANGE FIELD                    
                                                                                
VCANTX60 CHI   R5,1                QPIP FIELD?                                  
         JNE   VCANTX80            NO, CONTINUE                                 
         CLC   =C'QC',W4PROV       QPIP ONLY VALID FOR QUEBEC RESIDENTS         
         JE    VCANTX80                                                         
                                                                                
         CLI   5(R2),0             NOT QUEBEC AND FIELD HAS SOMETHING           
         JE    XIT                                                              
         J     ERINV               ERROR OUT                                    
                                                                                
VCANTX80 CLI   5(R2),0             MAKE SURE THERE IS SOMETHING                 
         JE    ERMIS                                                            
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),0             VALIDATE INPUT                               
         JNE   ERINV                                                            
         TM    4(R1),X'80'         NEGATIVES ARE NOT VALID                      
         JO    ERINV                                                            
                                                                                
         TM    W4STAT,W4STWKCN     WORKED IN CANADA?                            
         BO    VCANTX90                                                         
         CLC   TXBL7AMT,4(R1)      AMOUNT CANT EXCEED 70%(TAXABLE)              
         JL    ERAEX7TX                                                         
         J     VCANTX95                                                         
VCANTX90 CLC   TXBLAMNT,4(R1)      AMOUNT CANNOT EXCEED TAXABLE AMOUNT          
         JL    ERAEXTAX                                                         
VCANTX95 MVC   0(4,R3),4(R1)       SAVE TAX AMOUNT                              
                                                                                
         AHI   R2,L'LFDH+L'LFD     BUMP TO NEXT FIELD                           
         AHI   R3,4                BUMP TO NEXT TAX AMOUNT                      
         BCT   R5,VCANTX50                                                      
                                                                                
         J     XIT                                                              
         SPACE 3                                                                
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO TEST IF W4 IS CANADIAN                            *         
***********************************************************************         
         USING TAW4D,R4                                                         
W4CAN    NTR1  BASE=*,LABEL=*                                                   
         MVI   W4STAT,0                                                         
         MVC   W4PROV,SPACES                                                    
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         CLI   TAW4TYPE,TAW4TYCA   CANADIAN                                     
         JNE   NO                                                               
         OI    W4STAT,W4STCAN      MARK AS CANADIAN                             
         CLC   TAW4CP,SPACES                                                    
         BNH   W4CAN10                                                          
         OI    W4STAT,W4STW4CP     W4 HAS A CANADIAN PROVINCE                   
         MVC   W4PROV(L'TAW4CP),TAW4CP                                          
         J     W4CAN20                                                          
                                                                                
         USING TAA2D,R4                                                         
W4CAN10  L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         MVC   W4PROV,TAA2ST       SAVE PROVINCE                                
         OC    W4PROV,SPACES                                                    
                                                                                
W4CAN20  CLC   W4PROV,=C'QC '                                                   
         JNE   *+8                                                              
         OI    W4STAT,W4STQUEB     AND FROM QUEBEC                              
                                                                                
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',W4PROV)                                       
         JE    YES                                                              
         TM    W4STAT,W4STW4CP                                                  
         JO    YES                                                              
         OI    W4STAT,W4STERCP     ERROR WITH W4 CANADIAN PROV                  
                                                                                
         J     YES                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET TAXABLE AMOUNT                                *         
***********************************************************************         
                                                                                
GETTXBL  NTR1  BASE=*,LABEL=*                                                   
         XC    TXBLAMNT,TXBLAMNT                                                
                                                                                
         USING TATDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     GETTXBL5                                                         
                                                                                
GETTXBL3 BRAS  RE,NEXTEL                                                        
GETTXBL5 JNE   XIT                                                              
                                                                                
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TATDUNIT)                                     
         BNE   *+8                                                              
         OI    W4STAT,W4STWKCN                                                  
                                                                                
         MVC   PTYPE,TATDPMTY                                                   
                                                                                
         BRAS  RE,ISTXBL           SEE IF PAYTYPE IS TAXABLE                    
         JNE   GETTXBL3                                                         
                                                                                
         ICM   RF,15,TATDAMNT      SUM UP TAXABLE AMOUNTS                       
         A     RF,TXBLAMNT                                                      
         ST    RF,TXBLAMNT                                                      
         J     GETTXBL3                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET 70% OF TAXABLE AMOUNT                         *         
***********************************************************************         
                                                                                
TXBL70   NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         L     R1,TXBLAMNT                                                      
         LTR   R1,R1                                                            
         JZ    TXBL70X                                                          
                                                                                
         M     R0,=F'70'                                                        
         D     R0,=F'50'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
TXBL70X  ST    R1,TXBL7AMT                                                      
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO GET TAXABLE AMOUNT                                *         
***********************************************************************         
                                                                                
ISTXBL   NTR1  BASE=*,LABEL=*                                                   
         L     R5,AIO              SAVE AIO                                     
         MVC   AIO,AIO2                                                         
         MVC   BYTE,ELCODE         SAVE ELCODE                                  
                                                                                
         USING TLPMD,R3            FIND PAYMENT TYPE                            
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLPMCD,TLPMCDQ                                                   
         MVI   TLPMSCD,TLPMSCDQ                                                 
         MVC   TLPMPTYP,PTYPE                                                   
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(TLPMLEN-TLPMD),KEYSAVE                                       
         JNE   ISTXBLN                                                          
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAYDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAYDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   ISTXBLN                                                          
         TM    TAYDSTAT,TAYDSTAX   IS IT TAXABLE?                               
         JZ    ISTXBLN                                                          
                                                                                
ISTXBLY  XR    RF,RF                                                            
         B     *+8                                                              
ISTXBLN  LA    RF,1                                                             
         LTR   RF,RF                                                            
         ST    R5,AIO              RESTORE AIO                                  
         MVC   ELCODE,BYTE         RESTORE ELCODE                               
         J     XIT                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET TAAT ELEMENT                                  *         
***********************************************************************         
                                                                                
GETTAAT  NTR1  BASE=*,LABEL=*                                                   
         XC    CANTAX,CANTAX                                                    
                                                                                
         TM    PROSTAT,PSCHKCUT    WERE CHECKS CUT?                             
         BZ    GTAAT20                                                          
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLCKPCD,TLCKHCDQ    YES, GET CANADIAN TAX FROM TAAT ELEM         
         MVC   TLCKHCOM,TGCOM                                                   
         MVC   TLCKHSSN,SVSSN                                                   
         MVC   TLCKHINV,TGINV                                                   
         MVC   TLCKHSEQ,SVCASEQ                                                 
         GOTO1 HIGH                                                             
         B     GTAAT15                                                          
GTAAT10  GOTO1 SEQ                                                              
GTAAT15  CLC   KEY(TLCKHCSQ-TLCKPD),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         CLC   TLCKAGY,=C'999999'  DON'T GET IT FROM ADJUSTMENTS                
         BE    GTAAT10                                                          
                                                                                
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
                                                                                
GTAAT20  L     R4,AIO              LOOK FOR OVERALL CANADIAN TAX                
         MVI   ELCODE,TAATELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'CN ')                                            
         JNE   NO                                                               
                                                                                
         USING TAATD,R4                                                         
         L     R4,TGELEM                                                        
         MVC   CNTXFD,TAATTAX                                                   
                                                                                
         L     R4,AIO              LOOK FOR PROVINCE TAX                        
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         J     GTAAT50                                                          
                                                                                
GTAAT30  BRAS  RE,NEXTEL                                                        
GTAAT50  JNE   NO                                                               
         CLC   TAATUNIT,=C'CN '    SKIP CN                                      
         JE    GTAAT30                                                          
         MVC   CNTXPR,TAATTAX                                                   
         MVC   CNTXCPP,TAATPP                                                   
         MVC   CNTXEI,TAATEI                                                    
         MVC   CNTXQPIP,TAATPIP                                                 
                                                                                
         OI    W4STAT,W4STTAAT     W4 HAS TAAT ELEMENT IN TIMESHEET             
                                                                                
         J     YES                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAUSEPT                                                        
**********************************************************************          
*        SCREENS                                                     *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR71D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**********************************************************************          
*        DSECT TO COVER WORKING STORAGE                              *          
**********************************************************************          
                                                                                
MYD      DSECT                                                                  
MYACTNUM DS    XL(L'ACTNUM)                                                     
PROSTAT  DS    X                                                                
PSGOTORE EQU   X'80'               GO TO EMPLOYEE REACHED                       
PSINVPD  EQU   X'40'               INVOICE PAID                                 
PSCHKCUT EQU   X'20'               CHECK CUT                                    
                                                                                
LISTCHGD DS    C                                                                
                                                                                
W4STAT   DS    X                   W4 STATUS                                    
W4STCAN  EQU   X'80'               CANADIAN                                     
W4STQUEB EQU   X'40'               QUEBEC RESIDENT                              
W4STWKCN EQU   X'20'               WORKED IN CANADA                             
W4STTAAT EQU   X'10'               HAS TAAT ELEMENT IN TIMESHEET                
W4STW4CP EQU   X'08'               HAS W4 CANADIAN PROVINCE                     
W4STERCP EQU   X'04'               ERROR - W4 CANADIAN PROVINCE                 
                                                                                
W4PROV   DS    CL2                 W4 PROVINCE                                  
TSHTDSKA DS    A                   TIMESHEET DISK ADDRESS                       
TSHTKEY  DS    CL(L'KEY)           TIMESHEET KEY                                
                                                                                
PTYPESTA DS    X                   PAY TYPE STATUS                              
PTYPSTAX EQU   X'80'               TAXABLE                                      
                                                                                
PTYPE    DS    CL6                 PAYMENT TYPE                                 
                                                                                
TXBLAMNT DS    F                   TAXABLE AMOUNT                               
TXBL7AMT DS    F                   70% OF TAXABLE AMOUNT                        
CANTAX   DS    0XL20               CANADIAN TAXES                               
CNTXFD   DS    F                   FD                                           
CNTXPR   DS    F                   PR/QU                                        
CNTXCPP  DS    F                   CPP/QPP                                      
CNTXEI   DS    F                   EI/QEI                                       
CNTXQPIP DS    F                   QPIP                                         
                                                                                
SVKEY    DS    XL(L'KEY)                                                        
FRSTCAKY DS    XL(L'TLCAKEY)                                                    
SVCAKEY  DS    XL(L'TLCAKEY)                                                    
SVTMKEY  DS    XL(L'TLTMKEY)                                                    
                                                                                
LASTATE  DS    CL3                                                              
THISTATE DS    CL3                                                              
                                                                                
SVSSN    DS    CL9                                                              
SVPID    DS    CL6                                                              
SVCASEQ  DS    XL2                                                              
FLDWHDR  DS    XL(8+L'LNAME)                                                    
                                                                                
CURRITEM DS    X                   CURRENT ITEM                                 
CURRLIN  DS    A                   CURRENT LINE                                 
CURRLIST DS    A                   CURRENT LIST ENTRY                           
                                                                                
TOTAL    DS    F                                                                
OVERAMT  DS    F                                                                
                                                                                
ENTRIES  DS    XL(LINTBLNQ)       TABLE OF KEY & TAXABLE AMTS                   
                                                                                
SVCASTA3 DS    XL(L'TACASTA3)                                                   
                                                                                
ALINTAB  DS    A                                                                
                                                                                
ALSTPOAR DS    A                                                                
ATATDTAB DS    A                                                                
AUPDTSKS DS    A                                                                
AUPDPTYS DS    A                                                                
                                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
**********************************************************************          
*        DSECT TO COVER MAINTENANCE SCREEN LINE                      *          
**********************************************************************          
                                                                                
MAINTD   DSECT                                                                  
MTAXH    DS    XL8                                                              
MTAX     DS    XL3                 TAX UNIT                                     
MTASKH   DS    XL8                                                              
MTASK    DS    CL6                 TASK                                         
MPTYPH   DS    XL8                                                              
MPTYP    DS    CL6                 PAY TYPE                                     
         DS    XL4                                                              
MPTYPS   DS    XL1                 PAY TYPE STATUS                              
MPTYPSN  EQU   X'80'               AMOUNT WILL BE NEGATIVE                      
MPTYPAC  EQU   X'40'               AGENCY COMMISSION                            
         DS    XL3                                                              
MUNITH   DS    XL8                                                              
MUNIT    DS    CL6                 UNITS                                        
MRATEH   DS    XL8                                                              
MRATE    DS    CL10                RATE                                         
MAMTH    DS    XL8                                                              
MAMT     DS    CL10                AMOUNT                                       
MOVERH   DS    XL8                                                              
MOVER    DS    CL2                 OVERRIDE                                     
MLNQ     EQU   *-MAINTD                                                         
         EJECT                                                                  
**********************************************************************          
*        DSECT TO COVER LIST LINE                                    *          
**********************************************************************          
                                                                                
LISTD    DSECT                                                                  
LISTLDSP DS    XL2                 LINE DISPLACEMENT                            
LISTW4PR DS    CL2                 W4'S PROVINCE                                
LISTLDA  DS    A                   DISK ADDRESS                                 
LISTTXBL DS    F                   TAXABLE AMOUNT                               
LISTW4ST DS    CL1                 W4'S STATUS                                  
LISTLNQ  EQU   *-LISTD                                                          
                                                                                
**********************************************************************          
*        DSECT TO COVER LINE                                         *          
**********************************************************************          
                                                                                
LINED    DSECT                                                                  
LPIDH    DS    CL8                 PID                                          
LPID     DS    CL6                                                              
LPIDX    DS    CL8                                                              
LTAXBLH  DS    CL8                 TAXABLE                                      
LTAXBL   DS    CL10                                                             
LINSBLNQ EQU   *-LINED                                                          
LFDH     DS    CL8                 FD                                           
LFD      DS    CL10                                                             
LPRQUH   DS    CL8                 PR/QU                                        
LPRQU    DS    CL10                                                             
LCPPQPPH DS    CL8                 CPP/QPP                                      
LCPPQPP  DS    CL10                                                             
LEIQEIH  DS    CL8                 EI/QEI                                       
LEIQEI   DS    CL10                                                             
LQPIPH   DS    CL8                 QPIP                                         
LQPIP    DS    CL10                                                             
LNAMEH   DS    CL8                 NAME                                         
LNAME    DS    CL68                                                             
LINELNQ  EQU   *-LINED                                                          
                                                                                
**********************************************************************          
*        DSECT FOR NMOD STORAGE GRAB AREA                            *          
**********************************************************************          
                                                                                
TMPD     DSECT                                                                  
TATDTAB  DS    XL(5*TATDLNQ+1)                                                  
UPDTSKS  DS    XL(16*L'TLTKTASK+1)                                              
UPDPTYS  DS    XL(16*L'TLPMPTYP+1)                                              
TMPLNQ   EQU   *-TMPD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGENF2   12/30/13'                                      
         END                                                                    
