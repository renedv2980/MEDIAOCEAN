*          DATA SET PRWRI13    AT LEVEL 156 AS OF 07/17/02                      
*PHASE T40513A,*                                                                
*INCLUDE GETCOST                                                                
         TITLE 'CHANGE LOG'                                                     
*                                                                               
* BPLA 2/18/92  CHANGED PACKED FIELDS ON TAPE FROM PL11 TO PL6                  
*                                                                               
         TITLE 'T40513 - PHILIP MORRIS INVOICE TAPE'                            
T40513   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40513,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,ASAVE                                                         
         LA    RF,PTTAPE-SAVVALS(R1)                                            
         ST    RF,APTTAPE          NEW DCB ADDRESS                              
         LA    RF,PTFILE-SAVVALS(R1)                                            
         ST    RF,APTFILE          NEW DCB ADDRESS                              
*                                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      PRNTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
         BE    FINAL                                                            
         CLI   RPMODE,RPRUNLST     RUNLAST                                      
         BE    LST                                                              
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     CLI   TWAFIRST,0          TEST FIRST REQUEST                           
         BNE   INIT1                                                            
         ZAP   TAPECNT,=P'0'       YES-INITIALIZE TAPE COUNT                    
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
*                                                                               
         L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL         AND DCBS                                     
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVC   SVAGYCD,AGENCYCD                                                 
         MVC   SVTIT,TITLE                                                      
         MVC   SVSUBTIT,SUBTITLE                                                
         MVC   SVSPLID,SPOOLID                                                  
         MVCL  R0,RE                                                            
         TM    TOTVADOL+L'TOTVADOL-1,X'0C'                                      
         BO    INIT2                                                            
         B     INIT2                                                            
*                                                                               
INIT1    L     RE,ASAVE            NO-RESTORE SAVED VALUES                      
         LA    RF,SAVVALL1                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
INIT2    OI    PBQREAD,PBQRDBLS    READ BILL RECORDS                            
         CLI   OFFLINE,C'Y'        IF ONLINE BYPASS                             
         BNE   INOCLEAR                                                         
*                                                                               
         L     R1,AOV1WRK                                         L01           
         LA    R1,24(R1)                                          L01           
         MVC   0(28,R1),=C'** PRWRI13 BILLING  TABLE **'          L01           
         LA    R1,28(R1)                                          L01           
         ST    R1,AFORMTAB                                        L01           
         MVI   0(R1),X'FF'                                        L01           
********                                                                        
         MVI   MYFIRSTH,9          SET DRIVER'S FIRST HEADLINE                  
         XC    MONTAB,MONTAB       INITIALIZE MONTH TABLE                       
         XC    SVPRD,SVPRD                                                      
         XC    SVEST,SVEST                                                      
*                                                                               
*                                                                               
INOCLEAR CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
         LA    R2,PMTTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         OC    TITLE,SPACES                                                     
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    DS    0H                  TEST PRD=POL REQUEST                         
         LA    R2,PMTMONH          VALIDATE BILLING MONTH                       
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(2,WORK),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    EINV                                                             
         MVC   BILLMON,DUB         SAVE BILLING MONTH YYMM                      
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(3,PBQBLLST)    MONTH OF BILLING                 
         MVC   DUB+4(2),=C'31'                                                  
         GOTO1 (RF),(R1),DUB,(3,PBQBLLND)      START/END DATES                  
         MVC   PBQBST(6),PBQBLLST                                               
         MVI   PBQDATYP,C'L'                                                    
*****                                                                           
**                                                                              
         MVC   SUBTITLE(15),=C'MONTH REQUESTED'                                 
         GOTO1 DATCON,DMCB,DUB,(9,SUBTITLE+17)                                  
*****                                                                           
**                                                                              
         OC    SUBTITLE,SPACES                                                  
         GOTO1 CENTER,DMCB,SUBTITLE,36                                          
*                                                                               
         MVI   SUPTAP,C'N'                                                      
         LA    R2,PMTTAPH          OPTION TO SUPPRESS TAPE                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   SUPTAP,WORK                                                      
         CLI   SUPTAP,C'Y'                                                      
         BE    XIT                                                              
         CLI   SUPTAP,C'N'                                                      
         BNE   EINV                                                             
         B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
* PRINTIO INPUT HOOK                                                            
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   PBMODE,PBPROCCL     CLIENT FIRST                                 
         BNE   INP4                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   INP1                                                             
*                                                                               
         XC    SEQN4INV,SEQN4INV   CLEAR INVOICE SEQUENCE NUMBER                
*        LA    RE,BILLTAB          CLEAR BILL TABLE                             
         L     RE,AFORMTAB                                        L01           
         LH    RF,=Y(999*L'BILLTAB)                                             
         XCEF  ,                                                                
INP1     MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         MVI   AGENCYCD,C' '                                                    
         LA    R1,AGYTAB           FIND PM AGENCY CODE                          
*                                                                               
INP2     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         OI    ERRCD,ERRAGY                                                     
         B     EINV                WAS INP4                                     
         CLC   PBAGY(3),0(R1)                                                   
         BE    *+12                                                             
INP3     LA    R1,AGYTABL(R1)                                                   
         B     INP2                                                             
         CLC   PBCLT,3(R1)         MATCH ON CLIENT                              
         BNE   INP3                                                             
         MVC   PMACCT#,6(R1)                                                    
         MVC   AGENCYCD,2(R1)                                                   
         MVC   PMMEDIAC,12(R1)       MEDIA CODE                                 
         B     INP4                                                             
*                                                                               
AGYTAB   DC    CL2'BS',CL1'N',C'PM ',C'310774',C'PN'     BACKER - NEWS          
AGYTABL  EQU   *-AGYTAB                                                         
         DC    CL2'BS',CL1'M',C'PM ',C'310774',C'PM'     BACKER - MAGS          
         DC    CL2'BS',CL1'O',C'PM ',C'310774',C'OD'     BACKER - OUTD          
         DC    CL2'BS',CL1'S',C'PM ',C'310774',C'PM'     BACKER - SUPP          
         DC    CL2'SJ',CL1'N',C'PM ',C'310775',C'PN'     SJR - NEWS             
         DC    CL2'SJ',CL1'M',C'PM ',C'310775',C'PM'     SJR - NEWS             
         DC    CL2'YN',CL1'M',C'PMU',C'310775',C'PM'     SJR - NEWS             
         DC    X'00'                                                            
*                                                                               
INP4     CLI   OFFLINE,C'Y'                                                     
         BNE   INPX                                                             
*                                                                               
*                                                                               
         CLI   PBMODE,PBPROCBL     BILL RECORDS                                 
         BNE   INPX                                                             
*                                                                               
*                                                                               
INPX     B     XIT                                                              
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,33,ELCODE                                                     
         PRINT NOGEN                                                            
*                                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHKNTR  NTR1  ,                                                                
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     PUT TO SORT                                  
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
*                                                                               
*       AGENCY REPORT                                                           
*                                                                               
         DC    CL8'IMEDIAC ',A(IMEDIAC)   MEDIA CODE                            
         DC    CL8'OMEDIAC ',A(OMEDIAC)                                         
         DC    CL8'IINVOICE',A(IINVOICE)  INVOICE NUMBER                        
         DC    CL8'OINVOICE',A(OINVOICE)                                        
         DC    CL8'IEST    ',A(IEST)       ESTIMATE                             
         DC    CL8'OEST    ',A(OEST)       ESTIMATE                             
         DC    CL8'IPRD    ',A(IPRD)       PRODUCT                              
         DC    CL8'OPRD    ',A(OPRD)       PRODUCT                              
         DC    CL8'IGROSS  ',A(IGROSS)    GROSS AMOUNT                          
         DC    CL8'OGROSS  ',A(OGROS)                                           
         DC    CL8'IDISC   ',A(IDISC)     DISCOUNT                              
         DC    CL8'ODISC   ',A(ODISC)                                           
         DC    CL8'INCOST  ',A(INCOST)    NET COST                              
         DC    CL8'ONCOST  ',A(ONCOST)                                          
         DC    CL8'INCOMM  ',A(INCOMM)    NET COMMISSION                        
         DC    CL8'ONCOMM  ',A(ONCOMM)                                          
         DC    CL8'LASTCOL ',A(LASTCOL)                                         
**                                                                              
**                                        AGENCY TOTALS                         
**                                                                              
         DC    CL8'ITGROSS ',A(ITGROSS)  GROSS AMOUNT                           
         DC    CL8'OTGROSS ',A(OTGROS)                                          
         DC    CL8'ITDISC  ',A(ITDISC)   DISCOUNT                               
         DC    CL8'OTDISC  ',A(OTDISC)                                          
         DC    CL8'ITNCOST ',A(ITNCOST)  NET COST                               
         DC    CL8'OTNCOST ',A(OTNCOST)                                         
         DC    CL8'ITNCOMM ',A(ITNCOMM)  NET COMMISSION                         
         DC    CL8'OTNCOMM ',A(OTNCOMM)                                         
         DC    CL8'ITAPETOT',A(ITAPETOT)                                        
         DC    CL8'OTAPETOT',A(OTAPETOT)                                        
**                                                                              
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         MVI   GLOPTS+2,1          REPORT 1 = TAPE RECORD MAP                   
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     R6,PBGRS      PBGRS SHOULD HAVE ADDRESS OF BKELEM                
         USING BKELEM,R6                                                        
         L     R5,PBAIO1                                                        
         USING PBILLRCD,R5                                                      
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXEC2                                                            
*******  MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     R1,GLADTENT                                                      
*******  CLI   DRINLEV-DRIND(R1),1 TEST LEVEL 1                                 
*******  BH    EXEC2                                                            
*******  MVI   REJECT,C'N'         YES-RESET THE REJECT SWITCH                  
*******  MVI   MONEY,C'N'          RESET MONEY SWITCH                           
*                                                                               
EXEC2    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
*                                                                               
IMEDIAC  DS    0H                                                               
         MVC   0(L'PMMEDIAC,R2),PMMEDIAC                                        
         B     XIT                                                              
*                                                                               
OMEDIAC  DS    0H                                                               
         MVC   0(L'PMMEDIAC,R3),0(R2)                                           
         B     XIT                                                              
*                                                                               
IEST     DS    0H                                                               
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         UNPK  0(3,R2),DUB                                                      
         OI    2(R2),X'F0'                                                      
         MVC   PMEST,SPACES                                                     
         MVC   PMEST(3),0(R2)                                                   
         B     XIT                                                              
*                                                                               
OEST     DS    0H                                                               
         MVC   0(3,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
IPRD     DS    0H                                                               
         MVC   0(3,R2),PBILKPRD                                                 
         MVC   PMPRD,PBILKPRD                                                   
         B     XIT                                                              
*                                                                               
OPRD     DS    0H                                                               
         MVC   0(3,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
ITGROSS  ZAP   0(8,R2),TGROSS                                                   
         B     XIT                                                              
*                                                                               
ITDISC   ZAP   0(8,R2),TDSC                                                     
         B     XIT                                                              
*                                                                               
ITNCOST  ZAP   0(8,R2),TNET                                                     
         B     XIT                                                              
*                                                                               
ITNCOMM  ZAP   0(8,R2),TNCOMM                                                   
         B     XIT                                                              
*                                                                               
ITAPETOT DS    0H                                                               
         CLI   SUPTAP,C'Y'                                                      
         BE    XIT                                                              
         AP    0(8,R2),=P'2'    50 AND 55 RECORD                                
         B     XIT                                                              
*                                                                               
OTGROS   B     OGROS                                                            
*                                                                               
OTDISC   B     OGROS                                                            
*                                                                               
OTNCOST  B     OGROS                                                            
*                                                                               
OTNCOMM  B     OGROS                                                            
*                                                                               
OTAPETOT B     OGROS                                                            
*                                                                               
IINVOICE DS    0H                                                               
         MVC   PMINV(2),PBILLDAT        YEAR                                    
         MVC   PMINV+2(1),PBMED                                                 
         MVI   PMINV+3,C'-'                                                     
         MVC   PMINV+4(2),PBILLDAT+2 MONTH                                      
         MVI   PMINV+6,C'-'                                                     
         EDIT  (B2,PBILKBNO),(4,PMINV+7),FILL=0 INVOICE NUMBER                  
         MVC   0(13,R2),PMINV                                                   
         B     XIT                                                              
*                                                                               
OINVOICE DS    0H                                                               
         MVC   0(13,R3),0(R2)                                                   
         B     XIT                                                              
*                                                                               
*                                                                               
IGROSS   DS    0H                                                               
*                                                                               
* DETERMINE IF CD HAS BEEN SUBTRACTED FROM RECEIVABLE                           
         ZAP   WORK(8),=P'0'                                                    
         TM    PBILBASA,4          SEE IF FORMULA SUBTRACTED CD                 
         BNO   IGROSS1             DO NOT ADD IN CD                L01          
         ZAP   WORK(8),PBILLGRS      DETERMINE CD(GROSS)                        
         SP    WORK(8),PBILLBIL       LESS GROSS-CD   -- RESULT = CD            
IGROSS1  ZAP   DUB,PBILLRCV                                                     
         AP    DUB,WORK(8)      INCREASE RECEIVALBE BY CD                       
IGROSS5  ZAP   0(8,R2),DUB                                                      
         ZAP   PMGROSS,DUB                                                      
*                                                                               
         AP    TGROSS,DUB                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
OGROS    MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
IDISC    DS    0H                                                               
*                                                                               
IDISC5   ZAP   DUB,PBILLGRS       DETERMINE CD(GROSS)                           
         SP    DUB,PBILLBIL        LESS GROSS-CD   -- RESULT = CD               
IDISC10  ZAP   0(8,R2),DUB                                                      
         ZAP   PMDISC,DUB                                                       
*                                                                               
         AP    TDSC,DUB                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
ODISC    B     OGROS                                                            
*                                                                               
INCOST   DS    0H                                                               
*                                                                               
         ZAP   0(8,R2),PBILLNET   G-AC-CD                                       
         AP    0(8,R2),PMDISC    PLUS CD = NET                   L01            
*                                                                               
         ZAP   PMNET,0(8,R2)                                    L01             
*                                                                               
         AP    TNET,PMNET                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
ONCOST   B     OGROS                                                            
*                                                                               
INCOMM   DS    0H                                                               
         ZAP   0(8,R2),=P'0'          SET COMMISSION TO ZERO                    
         ZAP   PMNCOMM,=P'0'                                                    
*                                                                               
         AP    TNCOMM,PMNCOMM                                                   
*                                                                               
         B     XIT                                                              
*                                                                               
ONCOMM   B     OGROS                                                            
*                                                                               
*                                                                               
IERR     MVC   0(1,R2),ERRCD       ERRORS                                       
         B     XIT                                                              
*                                                                               
OERR     MVC   ERRCD,0(R2)                                                      
         CLI   ERRCD,0                                                          
         BE    XIT                                                              
         LR    R1,R3                                                            
         TM    ERRCD,ERRAGY                                                     
         BZ    *+14                                                             
         MVC   0(4,R1),=C'AGY,'                                                 
         LA    R1,4(R1)                                                         
         TM    ERRCD,ERRPRD                                                     
         BZ    *+14                                                             
         MVC   0(4,R1),=C'PRD,'                                                 
         LA    R1,4(R1)                                                         
         TM    ERRCD,ERREST                                                     
         BZ    *+14                                                             
         MVC   0(3,R1),=C'EST'                                                  
         LA    R1,3(R1)                                                         
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   XIT                                                              
         MVI   0(R1),C' '                                                       
         B     XIT                                                              
*                                                                               
*                                  LAST COLUMN - OUTPUT TAPE RECORD             
LASTCOL  LA    R6,PTREC                                                         
         L     R5,APTFILE                                                       
         XC    PTREC(4),PTREC                                                   
         MVC   PTREC(2),=H'79'                                                  
         MVC   PTREC+4(75),SPACES                                               
         XC    PMSORT,PMSORT                                                    
         CP    TAPECNT,=P'0'       TEST O/P FILES OPEN YET                      
         BH    LASTCOL4                                                         
         CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    LASTCOL4                                                         
         OPEN  ((R5),OUTPUT)       OPEN TAPE FILE                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LASTCOL4 DS    0H                                                               
*                                                                               
         XC    PTREC(4),PTREC                                                   
         MVC   PTREC(2),=H'79'                                                  
         MVC   PTREC+4(75),SPACES                                               
         XC    PMSORT,PMSORT                                                    
*                                                                               
*    BUILD 50 AND 55 RECORD                                                     
*                                                                               
         MVC   PTVEND,PMACCT#                                                   
         MVC   PTMEDC,PMMEDIAC                                                  
         MVI   PTDATA,C'C'                                                      
         MVC   PTINV,PMINV                                                      
         MVC   PTEST,PMEST                                                      
         MVC   PTRECT,=C'50'                                                    
         ZAP   PTGROSS,PMGROSS                                                  
         ZAP   PTDISC,PMDISC                                                    
         ZAP   PTNET,PMNET                                                      
         ZAP   PTNCOMM,PMNCOMM                                                  
         ZAP   PTPSF,=P'0'                                                      
*                                                                               
         MVC   PMSMEDC,PMMEDIAC                                                 
         MVC   PMSINV,PMINV                                                     
         MVC   PMSEST,PMEST                                                     
         MVC   PMSRECT,=C'50'                                                   
         CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    XIT                 DO NOT ADD TO TAPECOUNT                      
         AP    TAPECNT,=P'1'                                                    
         PUT   (R5),(R6)           PUT TO TAPE                                  
*                                                                               
*  CREATE 55 RECORD                                                             
*                                                                               
         MVC   PTPRD,PMPRD                                                      
         MVC   PTRECT,=C'55'                                                    
*                                                                               
         MVC   PMSPRD,PMPRD                                                     
         MVC   PMSRECT,=C'55'                                                   
*                                                                               
         PUT   (R5),(R6)           PUT TO TAPE                                  
ADDONE   AP    TAPECNT,=P'1'                                                    
         B     XIT                                                              
         EJECT                                                                  
* INPUT/OUPUT ROUTINES FOR AGENCY REPORT                                        
*                                                                               
         SPACE 1                                                                
*                                                                               
TOTAL    DS    0H        '         OPTION TO SUPPRESS TAPE                      
         MVC   0(7,R3),=C'**ALL**'                                              
         MVC   198(6,R3),=C'COUNT='                                             
         LA    R3,198+198(R3)                                                   
         UNPK  0(7,R3),TAPECNT     PRINT TAPE COUNT                             
         OI    6(R3),X'F0'                                                      
         B     XIT                                                              
         EJECT                                                                  
* DRIVER ABOUT TO PUT TO SORT                                                   
*                                                                               
PUTSRT    B    XIT                                                              
*PUTSRT   CLI   REJECT,C'Y'         TEST RECORD REJECTED                        
**        BE    *+12                                                            
*         CLI   MONEY,C'N'          OR $0                                       
*         BNE   *+8                                                             
*         MVI   GLHOOK,GLDONT       YES-TELL DRIVER TO REJECT                   
*         B     XIT                                                             
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
HEADHK   L     R2,AH4                                                           
         CLI   GLOPTS+2,1                                                       
         BNE   HEADRPT2                                                         
         MVC   448(15,R2),=C'VENDOR NUMBER= '                                   
         MVC   464(6,R2),PMACCT#                                                
         B     HEADHKX                                                          
*                                                                               
HEADRPT2 MVC   53(18,R2),=C'** AGENCY RECAP **'                                 
*                                                                               
HEADHKX  B     XIT                                                              
         EJECT                                                                  
* ABOUT TO PRINT A LINE                                                         
*                                                                               
*PRINT    CLI   GLOPTS+2,2          TEST AGENCY REPORT                          
*         BE    XIT                 YES-PRINT ALL LINES                         
*         CLI   REJECT,C'Y'         IF LINE REJECTED                            
*         BE    *+12                                                            
*         CLI   MONEY,C'N'          OR $0,                                      
*         BNE   *+8                                                             
*         MVI   GLHOOK,GLDONT       TELL DRIVER                                 
*         MVI   REJECT,C'N'                                                     
*         MVI   MONEY,C'N'                                                      
PRINT     B     XIT                                                             
         EJECT                                                                  
* FINAL HOOK                                                                    
*                                                                               
FINAL    DS    0H                                                               
*        LA    R5,BILLTAB                                                       
         L     R5,AFORMTAB                                        L01           
*        GOTO1 =V(PRNTBL),DMCB,(5,=C'TABL'),(R5),=C'DUMP',800,=C'1D'            
         CLI   0(R5),0       ANY DATA ENTERED D                   L01           
         BE    FINALA                                             L01           
         CLI   0(R5),255         FIRST TIME THRU U                L01           
         BE    FINALA                                                           
*                                                                               
FINALA   L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALL1                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVC   SVAGYCD,AGENCYCD                                                 
         MVC   SVSORT,PBAOFFBF   ADDRESS OF OFFLINE BUFFER                      
         MVC   SVTIT,TITLE                                                      
         MVC   SVSUBTIT,SUBTITLE                                                
         MVC   SVSPLID,SPOOLID                                                  
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
* RUNLAST                                                                       
*                                                                               
LST      DS    0H                                                               
         L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALL1                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         CLI   SUPTAP,C'Y'          SUPPRESS TAPE OUTPUT                        
         BE    XIT                                                              
         L     R5,APTFILE                                                       
         CLOSE ((R5))                                                           
         PRINT GEN                                                              
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         L     R5,APTFILE                                                       
         OPEN  ((R5),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*        OPEN  PTFILE          AS INPUT                                         
*                                                                               
GETANOTH DS    0H                                                               
         L     R5,APTFILE                                                       
         GET   (R5),PTREC                                                       
         GOTO1 SORTER,DMCB,=C'PUT',PTREC                                        
         B     GETANOTH                                                         
**                                                                              
ENDING   DS    0H                                                               
         L     R2,APTTAPE                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
LOOP     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    ENDSORTR                                                         
         L     R3,APTTAPE                                                       
         PUT   (R3),(R2)                                                        
         AP    OUTCOUNT,=P'1'                                                   
         B     LOOP                                                             
*                                                                               
ENDSORTR DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         L     R2,APTTAPE                                                       
         CLOSE ((R2))                                                           
         B     XIT                                                              
         PRINT NOGEN                                                            
**                                                                              
**                                                                              
         L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALL1                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         MVI   GLOPTS+2,3          2ND REPORT = AGENCY REPORT                   
         CP    TAPECNT,=P'0'       VERIFY TAPE OPEN                             
         BNH   LSTX                                                             
         GOTO1 INITDRIV            YES-INITIALIZE DRIVER                        
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,DRHKNTR                                                       
         ST    R1,GLAHOOK                                                       
*        OI    GLINDS,GLPALTOT                                                  
*        MVI   MYFIRSTH,8                                                       
*        MVI   GLMODE,GLINIT                                                    
*        GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
*                                                                               
LST2     DS    0H                                                               
*        MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
*        GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
* ONLY ONE RECORD                                                               
*                                                                               
LST90    MVC   TITLE,SVTIT         RESTORE TITLES                               
         MVC   SUBTITLE,SVSUBTIT                                                
         MVC   SPOOLID,SVSPLID                                                  
         GOTO1 OPENPQ              INITIALIZE SPOOL                             
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
LSTX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         DS    0F                                                               
BINSRCHT DC    F'0',F'0',A(0),A(20),A(12),A(999)                                
*                  TABLE COUNT LEN   DISP  TOT REC                              
*     PARAMETER  1  2      3    4     5    6                                    
ASAVE    DS    A                                                                
SEQN4INV DC    H'0'                                                             
TGROSS   DC    PL8'0'              TOTAL GROSS                                  
TDSC     DC    PL8'0'              TOTAL CD                                     
TNET     DC    PL8'0'              TOTAL NET                                    
TNCOMM   DC    PL8'0'              TOTAL COMMISSION                             
*                                                                               
******************* SORT STUFF ********************************                 
SORTCARD DC    CL80'SORT FIELDS=(80,33,A),FORMAT=BI,WORK=1' *                   
* *                                                                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=112 '               *                  
******************* SORT STUFF ********************************                 
PMACCT#  DS    CL6                 VENDOR NUMBER                                
PMMEDIAC DS    CL2                 MEDIA CODE                                   
PMINV    DS    CL13                INVOICE NUMBER                               
PMEST    DS    CL12                ESTIMATE NUMBER                              
PMPRD    DS    CL3                 PRODUCT                                      
PMGROSS  DS    PL5                 AM'T DUE BEFORE CD                           
PMDISC   DS    PL5                 DISCOUNT                                     
PMNET    DS    PL5                 NET COST                                     
PMNCOMM  DS    PL5                 NET COMMISSION                               
*                                                                               
ETEST    DS    CL1                TEST ESTIMATE BYTE X'80' = TEST               
EBFORM   DS    XL5                BILLING FORMULA                               
EBFDATE  DS    XL3                BILLING FORMUAL EFF DATE                      
AGENCYCD DS    CL1                                                              
*                                                                               
SVPRD    DS    CL3                                                              
SVEST    DS    XL2                                                              
SVBFEST  DS    XL2                                                              
REJECT   DS    CL1                                                              
MONEY    DS    CL1                                                              
BILLMON  DS    CL4                                                              
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRAGY   EQU   X'80'               AGENCY                                       
ERRPRD   EQU   X'40'               PRODUCT                                      
ERREST   EQU   X'20'               ESTIMATE                                     
*                                                                               
ZEROS    DC    CL8'00000000'                                                    
XFF      DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                                   
MONTHTR  DC    C'IJKLMNOPQRST'                                                  
*                JFMAMJJASOND   MONTHS                                          
*                                                                               
MONTAB   DS    25XL6               MONTH TABLE                                  
*                                  ENTRIES ARE BINARY YYMM FOLLOWED             
*                                  BY CHARACTERS YYMM                           
*                                  EXAMPLE - X'5801' C'8801'                    
*                                                                               
DDPM     DC    CL8'PTFILE'                                                      
*DSNBT    DC    CL20'PRTTAPE.PP0BTXX1'                                          
DSNPM    DC    CL20'BPLA.PM.PTFILE'                                             
TMPALLOC DC    XL6'000003000003'                                                
APTTAPE  DS    A                                                                
APTFILE  DS    A                                                                
         EJECT                                                                  
* VALUES SAVED BETWEEN REQUESTS                                                 
*                                                                               
SAVVALS  DS    0X                                                               
*                                                                               
SVSORT   DS    F                                                                
SVAGYCD  DS    CL1                                                              
SVTIT    DS    CL63                                                             
SVSUBTIT DS    CL36                                                             
SVSPLID  DS    CL3                                                              
SUPTAP   DS    CL1                                                              
*                                                                               
OUTCOUNT DC    PL4'0'                                                           
TAPECNT  DC    PL4'0'                                                           
TOTVADOL DS    PL8'0'             TOTAL DOLLARS FOR VA RECORD                   
TOTVAREC DS    PL8'0'             TOTAL VE RECORDS FOR VA RECORD                
*                                                                               
SAVVALL1 EQU   *-SAVVALS                                                        
*                                                                               
PTFILE   DCB   DDNAME=PTFILE,DSORG=PS,LRECL=112,BLKSIZE=1120,          X        
               MACRF=(GM,PM),RECFM=FB,EODAD=ENDING                              
*                                                                               
PTTAPE   DCB   DDNAME=PTTAPE,DSORG=PS,RECFM=VB,LRECL=408,              X        
               BLKSIZE=4084,MACRF=(GM,PM)                                       
*                                                                               
SAVVALSL EQU   *-SAVVALS                                                        
*                                                                               
         EJECT                                                                  
AFORMTAB DS    F               ADDRESS OF BILLING WORK AREA                     
*                                                                               
       ++INCLUDE PPPMVREC                                                       
         ORG                                                                    
*                                                                               
*  SORT KEY FOR PTREC   (ONLY ON PTFILE)                                        
*                                                                               
PMSORT   DS    0CL33                                                            
PMSMEDC  DS    CL2                                                              
PMSINV   DS    CL13                                                             
PMSEST   DS    CL12                                                             
PMSPRD   DS    CL3                                                              
PMSRECT  DS    CL2                                                              
PMSID    DS    CL1                                                              
         SPACE 2                                                                
*SVKEY    DS    CL(BTMON-BTKEY+L'BTMON)    SAVED KEY                            
         EJECT                                                                  
* PRODUCT AND ESTIMATE TABLES FOR BT CODES                                      
*                                                                               
TOTVEMED DS    CL1                                                              
TOTVEMM  DS    CL2                 BILL MONTH                                   
TOTVEINV DS    CL4                 INVOICE NUMBER                               
TOTVECL  DS    CL3                 CLIENT                                       
TOTVEDAT DS    XL3                 BILL DATE                                    
TOTVEDOL DS    PL5                 TOTAL BILL DOLLARS                           
TOTVEDDS DS    PL3                 TOTAL DDS INVOICES                           
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*PRWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*PGESTREC                                                                       
*PBKREC                                                                         
*PPCLRST                                                                        
*PGENGRP                                                                        
*PRWRIFFD                                                                       
         PRINT   ON   *************WAS OFF                                      
       ++INCLUDE PRWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE PPCLRST                                                        
       ++INCLUDE PGENGRP                                                        
         PRINT   ON                                                             
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
PBILLRCD DSECT                                                                  
       ++INCLUDE PBILLREC                                                       
         EJECT                                                                  
BTESTRD  DSECT                                                                  
       ++INCLUDE PGESTREC                                                       
PJOBRECD DSECT                                                                  
       ++INCLUDE PJOBREC                                                        
         EJECT                                                                  
PBKRECD  DSECT                                                                  
       ++INCLUDE PBKREC                                                         
       ++INCLUDE DDBKELEM                                                       
         EJECT                                                                  
       ++INCLUDE PRWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE PRWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRWRID7D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
         DSECT                                                                  
         DS    C'** PRWRI13 BILL TABLE**'                                       
         DS    0H                                                               
*                                                                               
BILTAB   DS   X'FF'                                                             
         ORG   BILTAB                                                           
BILLTAB  DS    999XL(L'TOTVEMM+L'TOTVEINV+L'TOTVECL+L'TOTVEDOL+3+3)             
BILLTLN  EQU   L'TOTVEMM+L'TOTVEINV+L'TOTVECL+L'TOTVEDOL+L'TOTVEDAT+3           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'156PRWRI13   07/17/02'                                      
         END                                                                    
