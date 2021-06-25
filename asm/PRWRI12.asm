*          DATA SET PRWRI12    AT LEVEL 133 AS OF 07/17/02                      
*PHASE T40512A,*                                                                
*INCLUDE GETCOST                                                                
         TITLE 'CHANGE LOG'                                                     
**********INCLUDE PRINT                                                         
**********INCLUDE PRNTBL                                                        
* BPLA 7/10/91  FIX BATCH NUMBER ON VA,VE,VM,VL RECS                            
*               NOW BILLING MONTH - WAS REQUEST DATE MONTH                      
*               FIX ACCOUNT NUMBER ON VM RECORD                                 
*               PAD WITH SPACES  - WAS MOVING 6 AND 7 BYTES FROM                
*               A FIELD 4 BYTES LONG (BTACCT#)                                  
*               ADD AND INCREMENT SUBLINE TO VL/VM RECORDS                      
*                                                                               
* BPLA 6/20/91  SPECIAL CODE FOR REBATE BILLS                                   
*               SET AMOUNT DUE BEFORE DISCOUNT TO ZERO                          
*               INVERT SIGN OF PBILLRCV AND SET INTO DISCOUNT                   
*               SET AMOUNT DUE TO REAL PBILLRCV                                 
*                                                                               
* BPLA 6/13/91  SKIP ALL ZERO INVOICES (EXCEPT REBATE BILLS)                    
*                                                                               
* ROSA 6/5/91   ADD NEW ENTRY AT AGYTAB FOR TRISH                  L02          
* ROSA 4/24/91  MAKE MODIFICATIONS BASED ON TRISH GERMAN'S MEMO    L01          
*               DATED 4/16/91 A- RECEIVABLE AMOUNT- DO NOT ADD IN CD            
*               B-SUPPRESS ZERO INVOICES  C-EXCLUDE PRODUCT BTL FOR             
*               CLIENT BTG)                                                     
         TITLE 'T40512 - BANKERS TRUST TAPE'                                    
T40512   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40512,RA                                                      
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
*                                                                               
         LA    RF,BTTAPE-SAVVALS(R1)                                            
         ST    RF,ABTTAPE          NEW DCB ADDRESS                              
         LA    RF,BTFILE-SAVVALS(R1)                                            
         ST    RF,ABTFILE          NEW DCB ADDRESS                              
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
*                                                                               
         ZAP   TAPECNT,=P'0'       YES-INITIALIZE TAPE COUNT                    
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
*                                                                               
         MVC   SVAGYCD,AGENCYCD                                                 
         MVC   SVSORT,PBAOFFBF   ADDRESS OF OFFLINE BUFFER                      
         MVC   SVTIT,TITLE                                                      
         MVC   SVSUBTIT,SUBTITLE                                                
         MVC   SVSPLID,SPOOLID                                                  
*                                                                               
         TM    TOTVADOL+L'TOTVADOL-1,X'0C'                                      
         BO    INIT0                                                            
*                                                                               
         ZAP   TOTVADOL,=P'0'       TOTAL DOLLARS BILLED                        
         ZAP   TOTVAREC,=P'0'       VA REC COUNT                                
*                                                                               
INIT0    DS    0H                                                               
*                                                                               
         L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL         AND DCBS                                     
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
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
         MVC   0(28,R1),=C'** PRWRI12 BILLING  TABLE **'          L01           
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
         LA    R2,BTTTITH          TITLE                                        
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
         LA    R2,BTTMONH          VALIDATE BILLING MONTH                       
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
         LA    R2,BTTTAPH          OPTION TO SUPPRESS TAPE                      
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
         LA    R1,AGYTAB           FIND BT AGENCY CODE                          
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
         MVC   BTACCT#,6(R1)                                                    
         MVC   AGENCYCD,2(R1)                                                   
         B     INP4                                                             
*                                                                               
AGYTAB   DC    CL2'DM',CL1'N',C'BTF',C'2210'   DOREMUS                          
AGYTABL  EQU   *-AGYTAB                                                         
         DC    CL2'DM',CL1'N',C'BTG',C'2210'                                    
         DC    CL2'DM',CL1'M',C'BTF',C'2230'                                    
         DC    CL2'DM',CL1'M',C'BTG',C'2230'                                    
         DC    CL2'SJ',CL1'M',C'GFT',C'1234'   FOR TEST                         
         DC    CL2'SJ',CL1'M',C'BL ',C'2210'   FOR TEST                         
         DC    CL2'SJ',CL1'M',C'CBX',C'2210'   FOR TEST                         
         DC    CL2'SJ',CL1'N',C'BTT',C'2210'   FOR TEST           L02           
         DC    X'00'                                                            
*                                                                               
INP4     CLI   OFFLINE,C'Y'                                                     
         BNE   INPX                                                             
*                                                                               
*                                                                               
         CLI   PBMODE,PBPROCBL     BILL RECORDS                                 
         BNE   INPX                                                             
*                                                                               
         CLC   PBAGY,=C'DM'                                        L01          
         BNE   INGETJOB                                            L01          
         L     RF,AIO1                                             L01          
         CLC   PBCLT,=C'BTG'                                       L01          
         BNE   SEEIFZER                                            L01          
         USING PBILLRCD,RF                                         L01          
         CLC   PBILKPRD,=C'BTL'                                    L01          
         BE    BYPASS                                              L01          
*-------> SEE IF THIS IS A ZERO RECEIVABLE INVOICE                 L01          
SEEIFZER DS    0H                                                  L01          
***   NO SKIP ALL ZERO INVOICES (NON-REBATE)                                    
         CLI   PBILLTYP,C'R'                                                    
         BE    INGETJOB                                                         
**                                                                              
         CP    PBILLRCV,=P'0'                                      L01          
         BNE   INGETJOB                                            L01          
***   CHECK FOR BILLED CD NO-OPED 6/13/91                                       
****     CP    PBILLGRS,PBILLBIL   NO CD                           L01          
****     BNE   INGETJOB                                            L01          
BYPASS   MVI   PBMODE,0            CHANGE MODE                     L01          
         B     XIT                                                 L01          
         DROP  RF                                                               
*                                                                               
*      READ JOB NUMBER                                                          
*                                                                               
INGETJOB BAS   RE,GETJOB      GET JOB RECORD (AD RECORD)                        
INP8D    DS    0H                                                               
*                                                                               
INP9     DS    0H                                                               
*                                                                               
INP10    B     INPX                                                             
*                                                                               
INPX     B     XIT                                                              
         EJECT                                                                  
* GET JOB RECORD                                                                
*                                                                               
GETJOB   NTR1  ,                                                                
***********                                                                     
************                                                                    
*                                                                               
         L     R6,AIO1           ADDRESS OF BILLREC                             
         USING PBILLRCD,R6                                                      
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING PJOBRECD,R5                                                      
         MVC   PJOBKEY(10),PBILLREC                                             
         MVI   PJOBKRCD,X'15'                                                   
         MVC   PJOBKJOB,PBILLJOB                                                
         CLC   LASTJOB,KEY        JOB CHANGE SINCE LAST                         
         BE    XIT                                                              
         MVC   LASTJOB,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE   TEST RECORD FOUND                              
         BE    GETJ1                                                            
         XC    BTXPENSE,BTXPENSE                                                
         MVC   BTCNTACT,=CL20'**** UNASSIGNED ****'                             
         B     GETJ5                                                            
*                                                                               
GETJ1    L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   BTXPENSE,PJOBBLCC   EXPENSE CODE                                 
         MVC   BTCNTACT,PJOBBLCC+L'BTXPENSE                                     
         CLC   BTCNTACT,SPACES                                                  
         BH    GETJ5                                                            
         MVC   BTCNTACT,=CL20'**** UNASSIGNED ****'                             
*                                                                               
GETJ5    MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                                                             
*                                                                               
         B     XIT                                                              
*                                                                               
LASTJOB  DS    CL25                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
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
         DC    CL8'ICONTACT',A(ICONTACT)  CONTACT                               
         DC    CL8'OCONTACT',A(OCONTACT)                                        
         DC    CL8'IINVOICE',A(IINVOICE)  AGENCY REPORT                         
         DC    CL8'OINVOICE',A(OINVOICE)                                        
         DC    CL8'IINVDATE',A(IINVDATE)  INVOICE DATE                          
         DC    CL8'OINVDATE',A(OINVDATE)                                        
         DC    CL8'IDESC   ',A(IDESCR)    DESCRIPTION                           
         DC    CL8'ODESC   ',A(ODESCR)                                          
         DC    CL8'IEXPCODE',A(IEXPCODE)  EXPENSE CODE                          
         DC    CL8'OEXPCODE',A(OEXPCODE)                                        
         DC    CL8'IACCODE ',A(IACCODE)   ACCOUNT CODE                          
         DC    CL8'OACCODE ',A(OACCODE)                                         
         DC    CL8'IDUELCD ',A(IDUELCD)   DUE LESS CD                           
         DC    CL8'ODUELCD ',A(ODUELCD)   DUE LESS CD                           
         DC    CL8'IDISC   ',A(IDISC)     DISCOUNT                              
         DC    CL8'ODISC   ',A(ODISC)                                           
         DC    CL8'ICOST   ',A(ICOST)     AMT DUE                               
         DC    CL8'OCOST   ',A(OCOST)                                           
         DC    CL8'ITLCD   ',A(ITLCD)                                           
         DC    CL8'OTLCD   ',A(OTLCD)                                           
         DC    CL8'LASTCOL ',A(LASTCOL)                                         
         DC    CL8'ITDSC   ',A(ITDSC)                                           
         DC    CL8'OTDSC   ',A(OTLCD)                                           
         DC    CL8'ITAMD   ',A(ITAMD)                                           
         DC    CL8'OTAMD   ',A(OTLCD)                                           
         DC    CL8'TOTAL   ',A(TOTAL)                                           
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
*                                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
*                                                                               
ICONTACT DS    0H                                                               
         MVC   0(L'BTCNTACT,R2),BTCNTACT                                        
         B     XIT                                                              
*                                                                               
OCONTACT MVC   29(L'BTCNTACT,R3),0(R2)                                          
         OC    29(L'BTCNTACT,R3),SPACES                                         
         MVC   0(8,R3),=C'CONTACT-'                                             
         B     XIT                                                              
*                                                                               
ITLCD    ZAP   0(8,R2),TLCD                                                     
         B     XIT                                                              
*                                                                               
ITDSC    ZAP   0(8,R2),TDSC                                                     
         B     XIT                                                              
*                                                                               
ITAMD    ZAP   0(8,R2),TAMD                                                     
         B     XIT                                                              
*                                                                               
OTLCD    B     ODUELCD                                                          
*                                                                               
OTDSC    B     ODUELCD                                                          
*                                                                               
OTAMD    B     ODUELCD                                                          
*                                                                               
*                                                                               
IACCODE  DS    0H                                                               
         MVC   0(4,R2),BTACCT#                                                  
         MVC   4(3,R2),SPACES                                                   
         B     XIT                                                              
*                                                                               
OACCODE  DS    0H                                                               
         MVC   0(7,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
IINVOICE DS    0H                                                               
         MVC   BTINV(1),PBMED                                                   
         MVC   BTINV+1(2),PBILLDAT+2 MONTH                                      
         MVC   TOTVEMM,PBILLDAT+2                                               
         EDIT  (B2,PBILKBNO),(4,BTINV+3),FILL=0 INVOICE NUMBER                  
         MVC   TOTVEINV,BTINV+3                                                 
         MVC   BTINV+7(3),PBCLT                                                 
         MVC   TOTVECL,PBCLT                                                    
         MVC   0(10,R2),BTINV                                                   
         B     XIT                                                              
*                                                                               
OINVOICE DS    0H                                                               
         MVC   0(10,R3),0(R2)                                                   
         B     XIT                                                              
*                                                                               
IINVDATE DS    0H                                                               
         MVC   0(6,R2),PBILLDAT                                                 
         MVC   BTINVD(4),PBILLDAT+2              MMDD                           
         MVC   BTINVD+4(2),PBILLDAT              YY                             
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(3,TOTVEDAT)                            
         B     XIT                                                              
*                                                                               
OINVDATE DS    0H                                                               
         GOTO1 DATCON,DMCB,(R2),(8,(R3))                                        
         B     XIT                                                              
*                                                                               
IDESCR   DS    0H                                                               
         MVI   0(R2),C'P'                                                       
         MVC   1(1,R2),PBMED                                                    
         MVC   2(3,R2),PBCLT                                                    
         MVC   5(3,R2),PBILKPRD                                                 
         LA    R1,8(R2)                                                         
         EDIT  (2,PBILKEST),(4,(R1)),FILL=0                                     
         MVC   12(6,R2),PBILLJOB                                                
         B     XIT                                                              
*                                                                               
ODESCR   DS    0H                                                               
         MVC   0(18,R3),0(R2)                                                   
         B     XIT                                                              
*                                                                               
IEXPCODE DS    0H                                                               
         MVC   0(L'BTXPENSE,R2),BTXPENSE                                        
         B     XIT                                                              
*                                                                               
OEXPCODE DS    0H                                                               
         MVC   0(L'BTXPENSE,R3),0(R2)                                           
         B     XIT                                                              
*                                                                               
IDUELCD  DS    0H                                                               
*                                                                               
* DETERMINE IF CD HAS BEEN SUBTRACTED FROM RECEIVABLE                           
*                                                                               
         ZAP   DUB,=P'0'                                                        
         CLI   PBILLTYP,C'R'         SEE IF REBATE BILL                         
         BE    IDUELCD5              SET TO ZERO                                
*                                                                               
         ZAP   WORK(8),=P'0'                                                    
         TM    PBILBASA,4                                                       
         B     IDUELCD1            DO NOT ADD IN CD                L01          
         ZAP   WORK(8),PBILLGRS      DETERMINE CD(GROSS)                        
         SP    WORK(8),PBILLBIL       LESS GROSS-CD   -- RESULT = CD            
IDUELCD1 ZAP   DUB,PBILLRCV                                                     
         AP    DUB,WORK(8)      INCREASE RECEIVALBE BY CD                       
IDUELCD5 ZAP   0(8,R2),DUB                                                      
         ZAP   BTAMTLCD,DUB                                                     
         AP    TLCD,DUB                                                         
         B     XIT                                                              
*                                                                               
ODUELCD  MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
IDISC    DS    0H                                                               
         CLI   PBILLTYP,C'R'          SEE IF REBATE BILL                        
         BNE   IDISC5                                                           
         ZAP   DUB,PBILLRCV                                                     
         CVB   R0,DUB                                                           
         LCR   R0,R0                                                            
         CVD   R0,DUB                                                           
         B     IDISC10                                                          
*                                                                               
IDISC5   ZAP   DUB,PBILLGRS       DETERMINE CD(GROSS)                           
         SP    DUB,PBILLBIL        LESS GROSS-CD   -- RESULT = CD               
IDISC10  ZAP   0(8,R2),DUB                                                      
         ZAP   BTDISCNT,DUB                                                     
         AP    TDSC,DUB                                                         
         B     XIT                                                              
*                                                                               
ODISC    B     ODUELCD                                                          
*                                                                               
ICOST    DS    0H                                                               
         CLI   PBILLTYP,C'R'        SEE IF REBATE BILL                          
         BNE   ICOST5                                                           
         ZAP   0(8,R2),PBILLRCV     JUST SET TO PBILLRCV                        
         ZAP   TOTVEDOL,0(8,R2)                                    L01          
*                                                                               
         ZAP   TOTVEDDS,=P'1'       SET DDS INVOICE COUNTER                     
         ZAP   BTAMTDUE,0(8,R2)                                    L01          
         AP    TAMD,PBILLRCV                                                    
         B     XIT                                                              
*                                                                               
ICOST5   ZAP   0(8,R2),PBILLRCV                                                 
         SP    0(8,R2),BTDISCNT    REDUCE RC BY CD                 L01          
         ZAP   TOTVEDOL,0(8,R2)                                    L01          
*                                                                               
         ZAP   TOTVEDDS,=P'1'       SET DDS INVOICE COUNTER                     
         ZAP   BTAMTDUE,0(8,R2)                                    L01          
         AP    TAMD,PBILLRCV                                                    
         B     XIT                                                              
*                                                                               
OCOST    B     ODUELCD                                                          
*                                                                               
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
*LASTCOL  CLI   REJECT,C'Y'         TEST RECORD REJECTED                        
*        BE    XIT                 YES                                          
*        CLI   MONEY,C'N'          TEST $0                                      
*        BE    XIT                 YES                                          
LASTCOL  L     R6,BTREC                                                         
         L     R5,ABTFILE                                                       
         MVC   BTREC(80),SPACES                                                 
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
*    ADD AMT TO TABLE (BILLTAB)                                                 
*                                                                               
         L    R1,TWADCONS                                                       
         USING TWADCOND,R1                                                      
         L     RF,TBINSRCH                                                      
         DROP  R1                                                               
******                                                                          
         L     R1,AFORMTAB                                                      
         ST    R1,BINSRCHT+4                                                    
         GOTO1 (RF),BINSRCHT,(1,TOTVEMM)                                        
*                                                                               
         OC    BINSRCHT(4),BINSRCHT                                             
         BNZ   *+6                                                              
         DC    H'0'                    TABLE FULL                               
         CLI   BINSRCHT,1              RECORD ADDED                             
         BE    LAST01                                                           
         L     RF,BINSRCHT                                                      
         AP    L'TOTVEMM+L'TOTVEINV+3+L'TOTVECL(L'TOTVEDOL,RF),TOTVEDOL         
         AP    17(3,RF),=P'1'        BUMP DDS INVOICE COUNT                     
LAST01   DS    0H                                                               
*        L      R2,0(R1)                                                        
*T       GOTO1 =V(PRNTBL),DMCB,(5,=C'TABL'),(R2),=C'DUMP',80,=C'1D'             
         MVC   BTREC(80),SPACES                                                 
*                                                                               
*    BUILD VL AND VM RECORD                                                     
*                                                                               
         MVC   BTVLID,=C'VL'                                                    
         MVI   BTVLACT,C'1'        ASSUME POSITIVE                              
         CP    TOTVEDOL,=P'0'                                                   
         BNL   *+8                                                              
         MVI   BTVLACT,C'2'                                                     
         MVC   BTVLCODE,=C'NY '                                                 
         MVI   BTVLBATN,C'D'                                                    
         MVC   BTVLBATN+1(2),BILLMON+2  BILLING MONTH                           
         MVI   BTVLPVNO,C'M'                                                    
         XR    RE,RE                                                            
         PACK  DUB,TOTVEMM                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'12'                                                        
         BH    *-2                                                              
         BCTR  RE,0                                                             
         LA    RF,MONTHTR(RE)                                                   
         MVC   BTVLPVNO+1(1),0(RF)                                              
*                                                                               
         MVC   BTVLSVNO,TOTVEINV        INVOICE NUMBER                          
*******  MVC   BTVLLIN#,=C'001'                                                 
         L     RF,BINSRCHT                                                      
*                              RF SHOULD POINT TO BILLTAB ENTRY                 
*                              FOR THIS INVOICE NUMBER                          
         UNPK  BTVLLIN#,17(3,RF)                                                
         OI    BTVLLIN#+2,X'F0'                                                 
         AP    TOTVADOL,TOTVEDOL    TOTAL DOLLARS BILLED                        
         UNPK  BTVLINVA,TOTVEDOL                                                
         OI    BTVLINVA+L'BTVLINVA-1,X'F0'                                      
*                                                                               
*   CREATE SORT KEY                                                             
*                                                                               
         XC    BTSORT,BTSORT                                                    
         MVC   BTSMED,PBMED                                                     
         MVC   BTSCLT,PBCLT                                                     
         MVC   BTSINV#,TOTVEINV                                                 
         MVC   BTSMON,TOTVEMM                                                   
         LH    RF,SEQN4INV         MUST CLEAR FOR EACH CLIENT                   
         LA    RF,1(RF)                                                         
         STH   RF,SEQN4INV                                                      
         MVC   BTSSEQ,SEQN4INV                                                  
         MVI   BTSID,0             VL ID                                        
         CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    XIT                 DO NOT ADD TO TAPECOUNT                      
         PUT   (R5),(R6)           PUT TO TAPE                                  
*                                                                               
*  CREATE VM RECORD                                                             
*                                                                               
         MVC   BTVMID,=C'VM'                                                    
         MVC   BTVMACT#,SPACES                                                  
         MVC   BTVMACT#(4),BTACCT#                                              
         MVC   BTVMCTR#,BTXPENSE                                                
         MVC   BTVMCTR#+L'BTVMCTR#(17),SPACES                                   
         MVI   BTVMDESC,C'P'                                                    
         MVC   BTVMDESC+1(1),PBMED                                              
         MVC   BTVMDESC+2(3),PBCLT                                              
*                                                                               
         DROP  R5                                                               
*                                                                               
** ** ** ** ** ** ** ** **                                                      
         USING PBILLRCD,RF                                                      
         L     RF,PBAIO1                                                        
** ** ** ** ** ** ** ** **                                                      
         MVC   BTVMDESC+5(3),PBILKPRD                                           
         MVI   BTVMTYPC,C'E'                                                    
         LA    R1,BTVMDESC+8                                                    
         EDIT  (2,PBILKEST),(4,(R1)),FILL=0                                     
         OC    BTVMID(80),SPACES                                                
         MVC   BTVMDESC+12(6),PBILLJOB                                          
         MVI   BTSID,0             VL ID                                        
** ** ** ** ** ** ** ** **                                                      
         DROP  RF                                                               
** ** ** ** ** ** ** ** **                                                      
         PUT   (R5),(R6)           PUT TO TAPE                                  
ADDONE   AP    TAPECNT,=P'1'                                                    
         B     XIT                                                              
         EJECT                                                                  
* INPUT/OUPUT ROUTINES FOR AGENCY REPORT                                        
*                                                                               
         SPACE 1                                                                
*                                                                               
TOTAL    CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    *+10                                                             
         AP    TAPECNT,=P'1'       ADD ONE TO TAPE COUNT FOR TRAILER            
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
         MVC  440(38,R2),=C'DIVISION: CORPORATE OPERATING SERVICES'             
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
*        CLI   BILLTAB,0       ANY DATA ENTERED                                 
*        BE    FINALA                                                           
*        CLI   BILLTAB,255         FIRST TIME THRU                              
*        BE    FINALA                                                           
*****                                  ******                                   
**    PREPARE VE RECORDS FOR THIS CLIENT    *                                   
*****                                  ******                                   
         BAS   RE,BUILDVE                                                       
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
*****                                                                           
*    CREATE THE VA RECORD                                                       
*****                                                                           
         MVC   BTVAID(80),SPACES                                                
         XC    BTSORT,BTSORT                                                    
         MVC   BTVAID,=C'VA'                                                    
         MVI   BTVABATN,C'D'                                                    
         MVC   BTVABATN+1(2),BILLMON+2   BILLING MONTH                          
         MVI   BTVAACT,C'1'                                                     
         CP    TOTVADOL,=P'0'                                                   
         BNL   *+8                                                              
         MVI   BTVAACT,C'2'                                                     
         MVC   BTVACODE,=C'NY '                                                 
         MVC   BTVABADT(2),RCDATE                                               
         MVC   BTVABADT+2(2),RCDATE+3                                           
         MVC   BTVABADT+4(2),RCDATE+6                                           
         MVC   BTVADIST,=C'DO'                                                  
         EDIT  (P8,TOTVAREC),(4,BTVA#OFI),FILL=0                                
         EDIT  (P8,TOTVADOL),(11,BTVAAMTD),FILL=0                               
         CP    TAPECNT,=P'0'       SEE IF ANY PREVIOUS REQ                      
         BH    YESRQSTD            HAD ANY TAPE REQUESTS                        
         CLI   SUPTAP,C'Y'          SUPPRESS TAPE OUTPUT                        
         BE    XIT                                                              
YESRQSTD DS    0H                                                               
         L     R5,ABTFILE                                                       
         PUT   (R5),BTREC                                                       
         CLOSE ((R5))                                                           
         PRINT GEN                                                              
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         L     R5,ABTFILE                                                       
         OPEN  ((R5),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*        OPEN  BTFILE          AS INPUT                                         
*                                                                               
GETANOTH DS    0H                                                               
         L     R5,ABTFILE                                                       
         GET   (R5),BTREC                                                       
         GOTO1 SORTER,DMCB,=C'PUT',BTREC                                        
         B     GETANOTH                                                         
**                                                                              
ENDING   DS    0H                                                               
         L     R2,ABTTAPE                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
LOOP     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    ENDSORTR                                                         
         L     R3,ABTTAPE                                                       
         PUT   (R3),(R2)                                                        
         AP    OUTCOUNT,=P'1'                                                   
         B     LOOP                                                             
*                                                                               
ENDSORTR DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         L     R2,ABTTAPE                                                       
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
         GOTO1 VINIDRIV            YES-INITIALIZE DRIVER                        
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
BUILDVE  NTR1                                                                   
         CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    XIT                                                              
BUIL1    CLI   0(R5),0                                                          
         BE    XIT                                                              
         AP    TOTVAREC,=P'1'       COUNT OF VE RECORDS                         
         MVC   BTVEID,=C'VE'                                                    
         MVI   BTVEACT,C'1'                                                     
         CP    12(5,R5),=P'0'                                                   
         BNL   *+8                                                              
         MVI   BTVEACT,C'2'                                                     
         MVC   BTVECODE,=C'NY '                                                 
         MVI   BTVEBATN,C'D'                                                    
         MVC   BTVEBATN+1(2),BILLMON+2    BILLING MONTH                         
         MVI   BTVLPVNO,C'M'                                                    
         XR    RE,RE                                                            
         PACK  DUB,TOTVEMM                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'12'                                                        
         BH    *-2                                                              
         BCTR  RE,0                                                             
         LA    RF,MONTHTR(RE)                                                   
         MVC   BTVEPVNO+1(1),0(RF)                                              
*                                                                               
         MVC   BTVESVNO,2(R5)           INVOICE NUMBER                          
         UNPK  BTVEAMTD,12(5,R5)                                                
         OI    BTVEAMTD+L'BTVEAMTD-1,X'F0'                                      
         MVC   BTVEINV#(1),PBMED           MEDIA                                
         MVC   BTVEINV#+1(9),0(R5)                                              
         LA    RE,9(R5)                                                         
         GOTO1 DATCON,DMCB,(3,(RE)),BTVEINVD+4                                  
         MVC   BTVEINVD(4),BTVEINVD+6       MOVE MMDD TO FRONT                  
         MVC   BTVEVEN#,=C'D44      '                                           
         MVC   BTVEVNAM,=CL10'DOREMUS'                                          
         MVI   BTVETYPC,C'3'                                                    
         MVC   BTVETYPC+1(18),SPACES                                            
         MVC   BTVEDATE(2),RCDATE                                               
         MVC   BTVEDATE+2(2),RCDATE+3                                           
         MVC   BTVEDATE+4(2),RCDATE+6                                           
*                                                                               
*   CREATE SORT KEY                                                             
*                                                                               
         XC    BTSORT,BTSORT                                                    
         MVC   BTSMED,PBMED                                                     
         MVC   BTSCLT,PBCLT                                                     
         MVC   BTSINV#,2(R5)                                                    
         MVC   BTSMON,0(R5)                                                     
         MVI   BTSID,0             VL ID                                        
         CP    TAPECNT,=P'0'                                                    
         BE    *-2                                                              
         PRINT GEN                                                              
         LA    R6,BTREC                                                         
         L     R2,ABTFILE                                                       
         PUT   (R2),(R6)           PUT TO TAPE                                  
         PRINT NOGEN                                                            
         LA    R5,BILLTLN(R5)                                                   
         CLI   0(R5),0                                                          
         BE    XIT                                                              
         B     BUIL1                                                            
*                                                                               
*        **********************                                                 
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
TOTNET   DC    PL8'0'                                                           
TLCD     DC    PL8'0'              TOTAL LESS CD                                
TDSC     DC    PL8'0'              TOTAL CD                                     
TAMD     DC    PL8'0'              TOTAL DUE                                    
TOTCOM   DC    PL8'0'                                                           
******************* SORT STUFF ********************************                 
SORTCARD DC    CL80'SORT FIELDS=(81,15,A),FORMAT=BI,WORK=1' *                   
* *                                                                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=95 '                *                  
******************* SORT STUFF ********************************                 
BTACCT#  DS    CL4                                                              
BTINV    DS    CL10                INVOICE NUMBER                               
BTINVD   DS    CL6                 INVOICE DATE                                 
BTCNTACT DS    CL20                BILLING CONTACT                              
BTXPENSE DS    CL6                 EXPENSE CODE                                 
BTAMTLCD DS    PL5                 AM'T DUE BEFORE CD                           
BTDISCNT DS    PL5                 DISCOUNT                                     
BTAMTDUE DS    PL5                 AMT DUE                                      
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
DDBT     DC    CL8'BTFILE'                                                      
*DSNBT    DC    CL20'PRTTAPE.PP0BTXX1'                                          
DSNBT    DC    CL20'ROSA.BT.BTFILE'                                             
TMPALLOC DC    XL6'000003000003'                                                
ABTTAPE  DS    A                   A(BTTAPE DCB IN SPFUSER)                     
ABTFILE  DS    A                   A(BTFILE DCB IN SPFUSER)                     
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
BTFILE   DCB   DDNAME=BTFILE,DSORG=PS,LRECL=95,BLKSIZE=950,            X        
               MACRF=(GM,PM),RECFM=FB,EODAD=ENDING                              
*                                                                               
BTTAPE   DCB   DDNAME=BTTAPE,DSORG=PS,LRECL=80,BLKSIZE=80,             X        
               MACRF=(GM,PM),RECFM=FB                                           
SAVVALSL EQU   *-SAVVALS                                                        
*                                                                               
         EJECT                                                                  
AFORMTAB DS    F               ADDRESS OF BILLING WORK AREA                     
BTREC    DS    0CL80           *** BT RECORD ***                                
*                                                                               
       ++INCLUDE PPBTVAREC                                                      
         ORG   BTREC                                                            
       ++INCLUDE PPBTVEREC                                                      
         ORG   BTREC                                                            
       ++INCLUDE PPBTVLREC                                                      
         ORG   BTREC                                                            
       ++INCLUDE PPBTVMREC                                                      
         ORG                                                                    
*                                                                               
*  SORT KEY FOR BTREC                                                           
*                                                                               
BTSORT   DS    0CL15                                                            
BTSMED   DS    CL1                                                              
BTSCLT   DS    CL3                                                              
BTSINV#  DS    CL6                                                              
BTSMON   DS    CL2                                                              
BTSSEQ   DS    CL2                                                              
BTSID    DS    CL1                                                              
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
*PRWRIFFD                                                                       
*PPCLRST                                                                        
*PGENGRP                                                                        
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
       ++INCLUDE PRWRID9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
         DSECT                                                                  
         DS    C'** PRWRI12 BILL TABLE**'                                       
         DS    0H                                                               
*                                                                               
BILTAB   DS   X'FF'                                                             
         ORG   BILTAB                                                           
BILLTAB  DS    999XL(L'TOTVEMM+L'TOTVEINV+L'TOTVECL+L'TOTVEDOL+3+3)             
BILLTLN  EQU   L'TOTVEMM+L'TOTVEINV+L'TOTVECL+L'TOTVEDOL+L'TOTVEDAT+3           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'133PRWRI12   07/17/02'                                      
         END                                                                    
