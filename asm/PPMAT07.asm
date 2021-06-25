*          DATA SET PPMAT07    AT LEVEL 079 AS OF 01/02/06                      
*PHASE T40207A                                                                  
***********************************************************************         
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE 04/19/04 RELINKED AND LOADED TO USE ENLARGED CRTABLE                     
*               IN PPMATPRTD                                                    
*                                                                               
* SMYE 03/02    FIX BUG FOR PRODUCT VARIOUS (***) - WAS                         
*               ONLY TESTING FOR BUYDATE AND BUYLINE TO DISTINGUISH             
*               DISCREPANCIES, THUS IF DIFFERENT PRODUCTS HAD THE SAME          
*               BUYDATE, THE DISCREPANCY ENTERED FOR ONE PRODUCT WOULD          
*               BE TOTALLED AND LISTED FOR EVERY PRODUCT WITH THIS DATE         
*                                                                               
* SMYE 02/02    FIX BUY LINE NUMBER DISPLAY IN PRTB103                          
*                                                                               
* SMYE 06/01    OPTIONALLY EXCLUDE "*" MATCHED BUYS FROM REPORT                 
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
*  TITLE: T40207 - REPORTING OF PRINT INVOICES --  PRINT ENGINE                 
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200), WHICH CALLS                  
*               DDGENCON (T00A30) WHICH CALLS PPMAT02 (T40202)                  
*               WHICH CALLS EITHER PPMAT06 OR PPMAT25 WHICH                     
*               CALLS THIS                                                      
*                                                                               
*  CALLS TO:    SPOOL                                                           
*                                                                               
*  SCREENS:     NONE                                                            
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
T40207   TITLE 'PPMAT07 - PRINT INVOICE REPORT- PRINT ENGINE'                   
T40207   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40207*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     RA,4(R1)                                                         
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,8(R1)                                                         
         USING SYSD,R9                                                          
         L     R8,12(R1)                                                        
         USING SPOOLD,R8                                                        
         L     R5,16(R1)           MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         L     R4,20(R1)           OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         ST    R3,RELO                                                          
*                                                                               
         B     PRTBUY                                                           
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PRINTS THE BUYS AND CORRESPONDING CORRECTIONS                    
***********************************************************************         
PRTBUY   DS    0H                                                               
*--------------------------------------------------------------                 
*         INITIALIZE SOME FIELDS                                                
*--------------------------------------------------------------                 
         MVC   HEDMESS,SPACES                                                   
         ZAP   BUYGRTOT,=P'0'      GROSS TOTAL FOR ALL BUYS                     
         ZAP   CRINVGR,=P'0'       GROSS TOTAL FOR CORRECTIONS                  
         ZAP   UMINVGR,=P'0'       GROSS TOTAL FOR UNMATCHED INVOICES           
         ZAP   UMBUYGR,=P'0'       GROSS TOTAL FOR UNMATCHED BUYS               
         ZAP   INVGR,=P'0'         GROSS TOTAL FOR ALL INVOICE ITEMS            
         ZAP   MINVGR,=P'0'        GROSS TOTAL FOR MATCHED INVOICES             
         XC    MINVNUM,MINVNUM     COUNTS OF ITEMS                              
         XC    CINVNUM,CINVNUM                                                  
         XC    UMBUYNUM,UMBUYNUM                                                
         XC    UMINVNUM,UMINVNUM                                                
         XC    MNET,MNET           NET TOTALS                                   
         XC    CNET,CNET                                                        
         XC    ANET,ANET                                                        
         XC    UMBUYNET,UMBUYNET                                                
         XC    UMINVNET,UMINVNET                                                
         XC    BUYNETOT,BUYNETOT                                                
         XC    MCD,MCD             CASH DISC TOTALS                             
         XC    CCD,CCD                                                          
         XC    UMBUYCD,UMBUYCD                                                  
         XC    UMINVCD,UMINVCD                                                  
         XC    ACSHDSC,ACSHDSC                                                  
         XC    MTAX,MTAX           TAX TOTALS                                   
         XC    CTAX,CTAX                                                        
         XC    UMBUYTAX,UMBUYTAX                                                
         XC    UMINVTAX,UMINVTAX                                                
*----------------------------------------------------------                     
*          WHAT TYPE OF MEDIA?                                                  
*----------------------------------------------------------                     
         XC    MEDTYPE,MEDTYPE     WHAT MEDIA TYPE                              
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   MEDTYPE,C'N'                                                     
*        CLI   QMED,C'S'           SUPPLEMENTS ARE LIKE NEWS PAPER              
*        BNE   *+8                                                              
*        MVI   MEDTYPE,C'N'                                                     
*----------------------------------------------------------                     
*          HEADLINES                                                            
*----------------------------------------------------------                     
         MVC   RATEHED,=C'GROSS RATE'                                           
         CLI   AMGRSNET,C'N'       SHOW NET AMOUNTS?                            
         BNE   *+10                                                             
         MVC   RATEHED,=C' NET RATE  '                                          
*                                                                               
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   PRTB10                                                           
         LA    R1,NHDSPECS         HEADLINE SPECS FOR NEWSPAPER MEDIA           
         ST    R1,SPECS                                                         
         LA    R1,NHDHOOK          HEAD HOOK FOR MEDIA N                        
         ST    R1,HEADHOOK                                                      
         ICM   RF,15,ABOX          BOXES?                                       
         B     PRTBHI                                                           
PRTB10   LA    R1,MHDSPECS         HEADLINE SPECS FOR MAGAZINES                 
         ST    R1,SPECS                                                         
         LA    R1,MHDHOOK          HEAD HOOK FOR MEDIA M                        
         ST    R1,HEADHOOK                                                      
         ICM   RF,15,ABOX          BOXES?                                       
*----------------------------------------------------------                     
*          GET BUYS AND PRINT THEM OUT                                          
*----------------------------------------------------------                     
PRTBHI   DS    0H                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    PRTBLP                                                           
         DC    H'0'                                                             
*                                                                               
PRTBNXTH DS    0H                                                               
         GOTO1 HIGH                                                             
PRTBNXTS GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PRTBLP                                                           
         DC    H'0'                                                             
*                                                                               
PRTBLP   DS    0H                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE   SAME BASICS?                     
         BNE   PRTUNMAT            PRINT UNMATCHED ITEMS                        
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   =C'ALL',QPRD                                                     
         BE    *+10                                                             
         CLC   =C'***',QPRD        VARIOUS PRD?                                 
         BE    *+20                                                             
         OC    QPRD,SPACES                                                      
         CLC   PBUYKPRD,QPRD                                                    
         BNE   PRTUNMAT                                                         
*                                                                               
         CLC   PBUYKPUB(4),BPUB        SAME PUB (ACROSS ZONE,EDT)               
         BE    PRTB05                                                           
         BNL   PRTB03                                                           
*                       IF PUB IS LOWER                                         
*                       PUT PUB IN KEY A TRY AGAIN                              
         MVC   PBUYKPUB(6),BPUB       PUB/ZONE/EDT                              
         XC    PBUYKDAT(9),PBUYKDAT   CLEAR DATE/EST/LINE                       
         B     PRTBHI                                                           
*                                                                               
PRTB03   CLC   =C'ALL',QPRD                                                     
         BE    *+10                                                             
         CLC   =C'***',QPRD        PRD VARIOUS?                                 
         BNE   PRTUNMAT                                                         
         XC    PBUYKPUB(15),PBUYKPUB   CLEAR PUB/DATE/EST/LINE                  
         MVI   PBUYKPUB,X'FF'      FORCE TO NEXT PRD                            
         B     PRTBHI                                                           
*                                                                               
PRTB05   CLI   ALLZONE,C'Y'                                                     
         BE    *+14                                                             
         CLC   PBUYKPUB(6),BPUB         SAME PUB,ZONE AND EDT                   
         BNE   PRTB03          SEE IF I NEED TO LOOK AT ANOTHER PRD             
*                                                                               
         XC    PUBEXP,PUBEXP                                                    
         GOTO1 VPUBEDIT,DMCB,(8,PBUYKPUB),(C'S',PUBEXP)                         
*                                                                               
BUYOK    DS    0H                                                               
         CLC   PBUYKDAT,INVSTDT    WITHIN PERIOD?                               
         BNL   PRTB30                                                           
         MVC   PBUYKDAT,INVSTDT                                                 
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     PRTBHI                                                           
*                                                                               
PRTB30   CLC   PBUYKDAT,INVENDDT                                                
         BNH   PRTB35                                                           
         CLI   ALLZONE,C'Y'                                                     
         BNE   *+12                                                             
         MVI   PBUYKDAT,X'FF'      FORCE TO NEXT ZONE/ED                        
         B     PRTBHI                                                           
*                                                                               
         CLC   =C'ALL',QPRD                                                     
         BE    *+10                                                             
         CLC   =C'***',QPRD        MUST BE ALL ZONE OR VAR PRD                  
         BNE   PRTUNMAT            ELSE GET OUT                                 
         XC    PBUYKPUB(15),PBUYKPUB    CLEAR PUB/DATE/EST/LINE                 
         MVI   PBUYKPUB,X'FF'      FORCE TO NEXT PRD                            
         B     PRTBHI                                                           
*                                                                               
PRTB35   OC    BEST,BEST           NO ESTIMATE GIVEN?                           
         BZ    PRTB100             NONE                                         
         CLC   PBUYKEST,BEST       DOES GIVEN EST MATCH EST IN KEY?             
         BNE   PRTBNXTS            NO, GET NEXT BUY RECORD                      
*                                                                               
PRTB100  OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE  (ACTIVE PRODUCT)          
         BNZ   PRTBNXTS            THEN SKIP THE RECORD   (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    PRTBNXTS                                                         
*                                                                               
*IF OPTION IS FOR "DISCREPANCIES ONLY" EXCLUDE * MATCHED BUYS                   
*                                                                               
         CLI   MYRPTOPT,C'N'       EXCLUDE "* MATCHED" BUYS ?                   
         BNE   PRTB101             NO - CONTINUE                                
         TM    PBDSTAT,X'40'       MATCHED ?                                    
         BZ    PRTB101             NO - CONTINUE                                
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL            INVOICE MATCH ELEMENT FOUND ?                
         BNE   PRTB101             NO - CONTINUE                                
         USING PBINVELM,R6         YES                                          
         CLC   PBINVNUM,INVNUMH    SAME INVOICE NUMBER ?                        
         BE    PRTBNXTS            YES - SKIP                                   
*                                                                               
PRTB101  DS    0H                                                               
         OC    SPECREP,SPECREP     SPECIAL REP GIVEN                            
         BZ    PRTB102                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'80'        SPECIAL REPS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   PRTBNXTS                                                         
         USING PBSREPEL,R6                                                      
         CLC   SPECREP,PBSREP                                                   
         BNE   PRTBNXTS                                                         
         B     PRTB103                                                          
*                                                                               
PRTB102  DS    0H                                                               
         L     R6,AIO              IF NO SPEC REP GIVEN                         
         MVI   ELCODE,X'80'        SPECIAL REPS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   PRTB103                                                          
         USING PBSREPEL,R6                                                      
         OC    PBSREP,PBSREP       THEN SKIP IF THERE IS A SPEC REP             
         BNZ   PRTBNXTS                                                         
         B     PRTB103                                                          
*                                                                               
PRTB103  L     R6,AIO              RESET                                        
         USING PBUYREC,R6                                                       
         MVC   FRSTKEY,KEY         SAVE LAST BUY KEY FOR PPMAT25                
*------------------------------------------------------------------             
*  OK BUY IS COOL TO PRINT -- DATE AND ZONE,EDT (NEWS AND MAGS)                 
*------------------------------------------------------------------             
         MVC   PRTNZE,PUBEXP+9                                                  
*                                                                               
*NOP*    GOTO1 DATCON,DMCB,(3,PBUYKDAT),(12,PRTNIDT)  PUT OUT DATE              
*                                                                               
         LA    R3,PRTNIDT                                                       
*                                                                               
         ST    R3,DMCB+4           DEFAULT IS TO PUT OUT FULL DATE              
         MVI   DMCB+4,12                                                        
*                                                                               
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+8                                                              
         MVI   DMCB+4,9            JUST PUT OUT MONTH/YEAR                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT)     SHOW THE BUY DATE OR MONTH          
*                                                                               
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+14                                                             
         XC    3(3,R3),3(R3)       DON'T WANT THE YEAR                          
         SH    R3,=H'2'      BACK-UP SO LINE NUMBER WILL FOLLOW MTH             
*                                                                               
         CLI   PBUYKLIN,1          IF NOT BUYLINE 1                             
         BE    SBUY10                                                           
         MVI   5(R3),C'-'          THEN SHOW ME THE BUYLINE                     
*                                                                               
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    SBUY04                                                           
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  7(1,R3),DUB+7(1)                                                 
         OI    7(R3),X'F0'                                                      
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,SBUYATAB(RF)                                                  
         MVC   6(1,R3),0(RF)                                                    
         B     SBUY10              DONE                                         
*                                                                               
SBUY04   OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R3),DUB                                                      
         CLI   6(R3),C'0'                                                       
         BNE   SBUY10                                                           
         MVC   6(2,R3),7(R3)                                                    
*                                                                               
SBUY10   DS    0H                                                               
*                                                                               
*                                  SAME SPOT FOR N AND M MEDIA                  
*------------------------------------------------------------------             
*                  ESTIMATE (NEWS AND MAGS)                                     
*------------------------------------------------------------------             
SKIPBLN  DS    0H                                                               
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   PRTB110                                                          
         OC    PBUYKEST,PBUYKEST                                                
         BZ    PRTB110A                                                         
         EDIT  (B2,PBUYKEST),(3,PRTNEST),FILL=0   PUT OUT ESTIMATE              
         B     PRTB110A                                                         
         OC    PBUYKEST,PBUYKEST                                                
         BZ    PRTB110A                                                         
PRTB110  EDIT  (B2,PBUYKEST),(3,PRTMEST),FILL=0                                 
*----------------------------------------------------------------               
*                CHECK FOR MONTHLY DATE                                         
*----------------------------------------------------------------               
PRTB110A MVI   ELCODE,X'20'        GET BUY DESCRIPTION ELEMENT                  
         BAS   RE,GETEL                                                         
         USING PBDELEM,R6                                                       
*                                                                               
*NOP*    CLI   PBDFREQ,C'M'        MONTHLY?                                     
*NOP*    BNE   PRTB111                                                          
*NOP*    MVC   PRTNIDT+3(2),=C'  ' BLANK OUT DAY                                
*----------------------------------------------------------------               
PRTB111  DS    0H                                                               
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   PRTB200                                                          
*--------------------------------------------------------------------           
*                      SIZE (NEWSPAPER ONLY)                                    
*--------------------------------------------------------------------           
         CLI   PBDSPACE,C' '       IF 1ST BYTE > SPACE?                         
         BNH   PRTB120                                                          
         CLI   PBDSPACE,X'FF'      3 PACKED FIELS FOLLOW?                       
         BNE   PRTB115                                                          
         EDIT  (P3,PBDSHOW),(8,PRTNSIZ),ALIGN=LEFT                              
         B     PRTB130A                                                         
*                                                                               
PRTB115  MVC   PRTNSIZ(8),PBDSPACE                                              
         B     PRTB130A                                                         
*                                                                               
PRTB120  CLI   PBDUIND,0           PRINT SIZE WITH NO DECIMALS?                 
         BE    PRTB121                                                          
         CLI   PBDUIND,C'L'                                                     
         BE    PRTB121                                                          
         CLI   PBDUIND,C'I'                                                     
         BNE   PRTB124                                                          
*                                                                               
PRTB121  EDIT  (P3,PBDUNITS),(8,PRTNSIZ),ALIGN=LEFT     YES                     
*                                                                               
         SR    R1,R1               THEN PUT AN 'I' AFTER THE NUMBER             
PRTB122  LA    R3,PRTNSIZ                                                       
         AR    R3,R1                                                            
         CLI   0(R3),C' '                                                       
         BE    PRTB123                                                          
         LA    R1,1(R1)                                                         
         CH    R1,=Y(L'PRTNSIZ)                                                 
         BL    PRTB122                                                          
         B     PRTB129                                                          
*                                                                               
PRTB123  CLI   PBDUIND,C'I'        IF INCHES                                    
         BE    *+12                                                             
         CLI   PBDUIND,X'89'                                                    
         BNE   PRTB129                                                          
*                                                                               
         MVI   0(R3),C'I'                                                       
         LA    R3,1(R3)                                                         
         B     PRTB129                                                          
*                                                                               
PRTB124  CLI   PBDUIND,X'89'       INCHES WITH 2 DECIMALS?                      
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P3,PBDUNITS),(8,PRTNSIZ),2,ALIGN=LEFT    YES                    
         SR    R1,R1                                                            
         B     PRTB122             GO BACK AND PUT AN 'I' AFTER NUMBER          
*                                                                               
PRTB129  OC    PBDCLMS,PBDCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PBDCLMS,=P'0'                                                    
         ZAP   DUB,PBDCLMS                                                      
         BZ    PRTB130A                                                         
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P8,DUB),(4,0(R3)),ALIGN=LEFT                                    
*---------------------------------------------------------------------          
*                     RATE (NEWS ONLY)                                          
*---------------------------------------------------------------------          
PRTB130A ZAP   P11,PBDCOS                                                       
*                                                                               
         CLI   PBDCOSIN,C'S'                                                    
         BE    PRTB131                                                          
*                                                                               
         CLI   AMGRSNET,C'N'       SHOW NET AMOUNTS?                            
         BNE   PRTB131                                                          
         ZAP   AMGRSAMT,P11        YES                                          
         ZAP   AMPERCTG,MYPUBAC                                                 
         BAS   RE,GRTONET                                                       
         ZAP   P11,AMNETAMT                                                     
*                                                                               
PRTB131  DS    0H                                                               
         CLI   PBDCOSTY,C'U'       UNIT COST                                    
         BNE   PRTB134                                                          
*                                                                               
         CP    PBDCOS,=P'0'        FREE?                                        
         BNE   *+14                                                             
         MVC   PRTNRTE(4),=C'FREE'  YES                                         
         B     PRTB137                                                          
*                                                                               
         LA    R3,PRTNRTE                                                       
*                                                                               
         CLI   PBDCOSIN,C'S'                                                    
         BNE   *+12                                                             
         MVI   PRTNRTE,C'S'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
         CP    P11,=P'-999999999'  GREATER THAN 9999.99999?                     
         BL    *+14                                                             
         CP    P11,=P'999999999'                                                
         BNH   PRTB133               NO, 5 DECIMALS                             
         ZAP   AMGRSAMT,P11                                                     
         SRP   AMGRSAMT,64-3,0                                                  
         ZAP   P11,AMGRSAMT(8)                                                  
*                                                                               
         CLI   PBDCOSIN,C'S'                                                    
         BE    PRTB131B                                                         
*                                                                               
PRTB131A EDIT  (P11,P11),(12,(R3)),2,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     PRTB137                                        2 DEC             
*                                                                               
PRTB131B EDIT  (P11,P11),(11,(R3)),2,ALIGN=LEFT,MINUS=YES                       
         B     PRTB137                                                          
*                                                                               
PRTB133  CLI   PBDCOSIN,C'S'                                                    
         BE     PRTB133B                                                        
*                                                                               
PRTB133A EDIT  (P11,P11),(12,(R3)),5,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     PRTB137                                        2 DEC             
*                                                                               
PRTB133B EDIT  (P11,P11),(11,(R3)),5,ALIGN=LEFT,MINUS=YES                       
         B     PRTB137                                                          
*                                                                               
PRTB134  LA    R3,PRTNRTE                                                       
         CLI   PBDCOSIN,C'S'                                                    
         BNE   PRTB134A                                                         
         MVI   0(R3),C'S'                                                       
         LA    R3,1(R3)                                                         
         B     PRTB135                                                          
*                                                                               
PRTB134A MVI   PRTNRTE,C'T'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
PRTB135  EDIT  (P11,P11),(11,(R3)),2,ALIGN=LEFT,MINUS=YES                       
*--------------------------------------------------------------------           
*                      PREMIUM (NEWS ONLY)                                      
*--------------------------------------------------------------------           
PRTB137  DS    0H                                                               
         CLI   PBDCL,0             NO COLORS?                                   
         BE    PRTB141                                                          
*                                                                               
         EDIT  (B1,PBDCL),(1,PRTNPRM)                                           
         MVC   PRTNPRM+1(2),=C'C/'                                              
         LA    R1,PRTNPRM+3                                                     
         EDIT  (P5,PBDPRCOS),(8,0(R1)),2,ALIGN=LEFT,MINUS=YES                   
         B     PRTB150                                                          
*                                                                               
PRTB141  CP    PBDPRCOS,=P'0'                                                   
         BE    PRTB150                                                          
*                                                                               
         EDIT  (P5,PBDPRCOS),(11,PRTNPRM),2,ALIGN=LEFT,MINUS=YES                
*-------------------------------------------------------------------            
* CASH DISCOUNT, TEAR SHEET, PAID, BILLED, INSERTION (NEWS ONLY)                
*-------------------------------------------------------------------            
PRTB150  DS    0H                                                               
         CP    PBDCD,=P'0'         NO CASH DISCOUNT?                            
         BE    *+12                                                             
         MVI   PRTNPBC,C'Y'        THERE IS                                     
         B     *+8                                                              
         MVI   PRTNPBC,C'N'        NONE                                         
*                                                                               
         TM    PBDSTAT,X'10'       TEAR SHEET?                                  
         BZ    *+8                                                              
         MVI   PRTNPBC+1,C'T'                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        SEE IF WE HAVE A PAID ITEM                   
         BAS   RE,GETEL                                                         
PRTB155  BNE   PRTB160                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     PRTB155                                                          
***      BZ    PRTB160                                                          
         MVI   PRTNPBC+2,C'P'                                                   
*                                                                               
PRTB160  L     R6,AIO              BILLED                                       
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRTB161A                                                         
         USING PBILELEM,R6                                                      
         OC    PBLDATE,PBLDATE                                                  
         BZ    PRTB161A                                                         
         MVI   PRTNPBC+3,C'B'                                                   
*                                                                               
PRTB161A L     R6,AIO                                                           
         MVI   ELCODE,X'70'        INSERTION ORDER                              
         BAS   RE,GETEL                                                         
         BNE   PRTB161                                                          
         USING PIOELEM,R6                                                       
         OC    PIODATE,PIODATE                                                  
         BZ    PRTB161                                                          
         MVI   PRTNPBC+4,C'I'                                                   
*--------------------------------------------------------------------           
*       GROSS, NET, CD, AND PAY/BILL DATES  AND PRD(NEWS ONLY)                  
*--------------------------------------------------------------------           
PRTB161  L     R2,AIO                                                           
*                                                                               
         USING PBUYREC,R2                                                       
         MVC   PRTNPRD,PBUYKPRD                                                 
         GOTO1 GETINS,DMCB,AIO,GETINSA,PBUYKPRD,INVSTDT PAID/BLLD INFO          
         DROP  R2                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
*                                                                               
         MVC   BUYTAX,TAX          FOR TAX TOTAL                                
*                                                                               
         EDIT  (B4,GROSS),(11,PRTNGRS),FILL=0  JUST TO GET IN EBCIDIC           
         PACK  WKSPACE(8),PRTNGRS(11)                                           
         ZAP   DUMMY(8),WKSPACE(8)                                              
         AP    BUYGRTOT(8),WKSPACE(8)                                           
         EDIT  (B4,GROSS),(11,PRTNGRS),2,ALIGN=RIGHT,MINUS=YES                  
         L     R1,GROSS                                    FOR NOW              
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         MVC   TEMPBNET,FULL                                                    
         L     R1,BUYNETOT                                                      
         A     R1,TEMPBNET                                                      
         ST    R1,BUYNETOT                                                      
         EDIT  (B4,FULL),(11,PRTNNET),2,ALIGN=RIGHT,MINUS=YES                   
*                                  CASH DISCOUNT                                
         MVC   TEMPBCD,CSHDSC                                                   
         OC    CSHDSC,CSHDSC                                                    
         BNZ   PRTB163                                                          
         MVC   PRTNCD+3(4),=C'NONE'                                             
         B     PRTB165                                                          
PRTB163  DS    0H                                                               
         EDIT  (B4,CSHDSC),(8,PRTNCD),2,ALIGN=RIGHT,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
*                                  PAYABLE DATE                                 
PRTB165  L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(11,PRTNPDT)                            
*                                  BILLABLE DATE                                
         GOTO1 DATCON,DMCB,(3,PBDBDATE),(11,PRTNBDT)                            
*                                                                               
         B     PRTB300                                                          
         DROP  R3                                                               
*                                                                               
SBUYATAB DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
******************************************************************              
         EJECT                                                                  
*---------------------------------------------------------------------          
*           SPACE  (MAGS ONLY)                                                  
*---------------------------------------------------------------------          
PRTB200  DS    0H                  SOLELY FOR MAGAZINES                         
         USING PBDELEM,R6                                                       
         LA    R3,PRTMSPC                                                       
*                                                                               
         CLI   QMED,C'O'           OUTDOORS?                                    
         BNE   PRTB204             NO                                           
*                                                                               
         CLI   PBDSPACE,X'FF'      3 BYTES PACKED FOLLOWS?                      
         BNE   PRTB204                                                          
*                                                                               
         CP    PBDSHOW,=P'0'       NO VALUES IN SHOW/REG/ILLUM?                 
         BNE   PRTB201                                                          
         CP    PBDREG,=P'0'                                                     
         BNE   PRTB201                                                          
         CP    PBDILLUM,=P'0'                                                   
         BE    PRTB202             YES, GET COMMENT INSTEAD                     
*                                                                               
PRTB201  XC    PRTMSPC,PRTMSPC                                                  
         MVC   0(4,R3),=C'SRI='    DISPLAY SHOW/REG/ILLUM                       
         LA    R3,4(R3)                                                         
         CP    PBDSHOW,=P'99999'   DISPLAY 'SPC'                                
         BNE   PRTB201A                                                         
         MVC   0(3,R3),=C'SPC'                                                  
         LA    R3,3(R3)                                                         
         B     PRTB201B                                                         
PRTB201A EDIT  (P3,PBDSHOW),(3,0(R3)),ALIGN=LEFT                                
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
PRTB201B MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P3,PBDREG),(4,0(R3)),ALIGN=LEFT                                 
         LA    R3,3(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P3,PBDILLUM),(4,0(R3)),ALIGN=LEFT                               
         B     PRTB205                                                          
*                                                                               
PRTB202  L     R6,AIO                                                           
         MVI   ELCODE,X'66'        GET FIRST COMMENT                            
         BAS   RE,GETEL                                                         
         BE    PRTB203                                                          
*                                                                               
PRTB202A L     R6,AIO                                                           
         MVI   ELCODE,X'20'        RESTORE WHERE WE WERE BEFORE COMMENT         
         BAS   RE,GETEL                                                         
         B     PRTB205                                                          
*                                                                               
         USING PCOMELEM,R6                                                      
PRTB203  ZIC   R1,PCOMELEM+1                                                    
         SH    R1,=H'2'                                                         
         CH    R1,=Y(L'PRTMSPC)                                                 
         BNH   *+14                                                             
         MVC   0(L'PRTMSPC,R3),PCOMELEM+2    SEE PPBUY05, LABEL FMTCOM          
         B     PRTB202A                                                         
*                                                                               
         CH    R1,=H'2'                                                         
         BNH   PRTB202A                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PCOMELEM+2                                               
         B     PRTB202A                                                         
*                                                                               
         USING PBDELEM,R6                                                       
PRTB204  MVC   PRTMSPC,PBDSPACE                                                 
*                                                                               
*---------------------------------------------------------------------          
* CASH DISCOUNT, TEAR SHEET, PAID, BILLED, INSERTION (MAGS ONLY)                
*---------------------------------------------------------------------          
PRTB205  MVI   PRTMPBC,C'N'        CASH DISC                                    
         CP    PBDCD,=P'0'                                                      
         BE    *+8                                                              
         MVI   PRTMPBC,C'Y'                                                     
*                                                                               
         TM    PBDSTAT,X'10'       TEAR SHEET?                                  
         BZ    *+8                                                              
         MVI   PRTMPBC+1,C'T'                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        SEE IF WE HAVE A PAID ITEM                   
         BAS   RE,GETEL                                                         
PRTB207  BNE   PRTB210                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     PRTB207                                                          
***      BZ    PRTB210                                                          
         MVI   PRTMPBC+2,C'P'                                                   
*                                                                               
PRTB210  L     R6,AIO              BILLED                                       
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRTB211A                                                         
         USING PBILELEM,R6                                                      
         OC    PBLDATE,PBLDATE                                                  
         BZ    PRTB211A                                                         
         MVI   PRTMPBC+3,C'B'                                                   
*                                                                               
PRTB211A L     R6,AIO                                                           
         MVI   ELCODE,X'70'        INSERTION ORDER                              
         BAS   RE,GETEL                                                         
         BNE   PRTB211                                                          
         USING PIOELEM,R6                                                       
         OC    PIODATE,PIODATE                                                  
         BZ    PRTB211                                                          
         MVI   PRTMPBC+4,C'I'                                                   
*--------------------------------------------------------------------           
*        GROSS, NET, CD, PAY/BILL DATES AND PRD(MAGS ONLY)                      
*--------------------------------------------------------------------           
PRTB211  L     R2,AIO                                                           
         USING PBUYREC,R2                                                       
         MVC   PRTMPRD,PBUYKPRD                                                 
         GOTO1 GETINS,DMCB,AIO,GETINSA,PBUYKPRD,INVSTDT   PD/BLLD INFO          
         DROP  R2                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
*                                                                               
         MVC   BUYTAX,TAX          FOR TAX TOTAL                                
*                                                                               
         EDIT  (B4,GROSS),(11,PRTMGRS),FILL=0                                   
         PACK  WKSPACE(8),PRTMGRS(11)                                           
         ZAP   DUMMY(8),WKSPACE(8)                                              
         AP    BUYGRTOT(8),WKSPACE(8)                                           
         EDIT  (B4,GROSS),(11,PRTMGRS),2,ALIGN=RIGHT,MINUS=YES                  
         L     R1,GROSS                                    FOR NOW              
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         MVC   TEMPBNET,FULL                                                    
         L     R1,BUYNETOT                                                      
         A     R1,TEMPBNET                                                      
         ST    R1,BUYNETOT                                                      
         EDIT  (B4,FULL),(11,PRTMNET),2,ALIGN=RIGHT,MINUS=YES                   
*                                  CASH DISCOUNT                                
         MVC   TEMPBCD,CSHDSC                                                   
         OC    CSHDSC,CSHDSC                                                    
         BNZ   PRTB220                                                          
         MVC   PRTMCD+3(4),=C'NONE'                                             
         B     PRTB225                                                          
PRTB220  EDIT  (B4,CSHDSC),(8,PRTMCD),2,ALIGN=RIGHT,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
*                                  PAYABLE DATE                                 
PRTB225  L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(11,PRTMPDT)                            
*                                  BILLABLE DATE                                
         GOTO1 DATCON,DMCB,(3,PBDBDATE),(11,PRTMBDT)                            
*                                                                               
         DROP  R3,R6                                                            
********************************************************************            
         EJECT                                                                  
*--------------------------------------------------------------------           
*        INDICATE IF MATCHED (BOTH NEWS AND MAGS)                               
*--------------------------------------------------------------------           
PRTB300  DS    0H                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         TM    PBDSTAT,X'40'       IF MATCHED                                   
         BZ    PRTC400             NOT , CHECK FOR CORRECTION                   
*                                                                               
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         MVC   PRTNIND,=C'PM'                                                   
         B     PRTB325             NO CORRECTIONS IF MATCHED                    
         USING PBINVELM,R6                                                      
         CLC   PBINVNUM,INVNUMH                                                 
         BE    PRTB325                                                          
*                                                                               
         CLI   MEDTYPE,C'N'                                                     
         BNE   *+20                                                             
         MVC   PRTNIND,=C'PM'                                                   
         MVC   PRTNINVN,PBINVNUM                                                
         B     *+16                                                             
         MVC   PRTMIND,=C'PM'                                                   
         MVC   PRTMINVN,PBINVNUM                                                
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         B     PRTBANN             NO CORRECTIONS IF MATCHED                    
*                                                                               
PRTB325  L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         L     R1,MINVNUM          MATCHED INVOICE COUNT                        
         LA    R1,1(R1)                                                         
         ST    R1,MINVNUM                                                       
*                                                                               
         L     R1,MCD              MATCHED CD  TOTAL                            
         A     R1,TEMPBCD                                                       
         ST    R1,MCD                                                           
*                                                                               
         L     R1,MNET             MATCHED NET TOTAL                            
         A     R1,TEMPBNET                                                      
         ST    R1,MNET                                                          
*                                                                               
         L     R1,MTAX             MATCHED TAX TOTAL                            
         A     R1,BUYTAX                                                        
         ST    R1,MTAX                                                          
*                                                                               
         AP    MINVGR(8),DUMMY(8)  MATCHED GROSS TOTAL                          
*                                                                               
         CLC   =C'PM',PRTNIND                                                   
         BE    *+8                                                              
         MVI   PRTNIND+1,C'*'        AND PUT OUT THE ASTERISK                   
         B     PRTBANN                                                          
*                                                                               
*                                                                               
********************************************************************            
         EJECT                                                                  
*--------------------------------------------------------------------           
*        UNMATCHED BUY, CHECK FOR ASSOCIATED CORRECTION                         
*--------------------------------------------------------------------           
PRTC400  DS    0H                                                               
         MVI   GOTCORR,C'N'        INIT                                         
*                                                                               
         LA    R2,CRTABLE                                                       
         USING CORRTBLD,R2                                                      
*                                                                               
PRTC405  L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLC   PBUYKLIN,CORRLINE   MUST HAVE SAME BUYLINE                       
         BNE   PRTC490                                                          
         CLC   PBUYKDAT,CORBDATE   SAME DATE                                    
         BNE   PRTC490                                                          
         CLC   PBUYKEST,CORBEST    AND SAME ESTIMATE                            
         BNE   PRTC490                                                          
         CLC   PBUYKZON,CORRZONE   AND SAME ZONE                                
         BNE   PRTC490                                                          
         CLC   PBUYKEDT,CORREDTN   AND SAME EDITION                             
         BNE   PRTC490                                                          
*                                                                               
         CLC   =C'***',QPRD        PRODUCT VARIOUS ?                            
         BNE   PRTC406             NO                                           
         CLC   PBUYKPRD,CORRPRD    MUST HAVE SAME PRODUCT CODE                  
         BNE   PRTC490                                                          
*                                  PRINT CORRESPONDING CORRECTION               
PRTC406  MVC   PRTNIND,=CL2'->'                                                 
*                                                                               
         MVC   VALUIND,PBDUIND     SAVE BUY VALUES                              
         ZAP   VALUNITS,PBDUNITS                                                
         ZAP   VALCLMS,PBDCLMS                                                  
         MVC   VALCOSTY,PBDCOSTY                                                
         ZAP   VALCOST,PBDCOS                                                   
         MVC   VALCL,PBDCL                                                      
         ZAP   VALPRCOS,PBDPRCOS                                                
         MVC   VALSPACE,PBDSPACE                                                
         CLI   QMED,C'O'           OUTDOORS?                                    
         BNE   PRTC409                                                          
         CP    PBDSHOW,=P'0'                                                    
         BNE   PRTC409                                                          
         CP    PBDREG,=P'0'                                                     
         BNE   PRTC409                                                          
         CP    PBDILLUM,=P'0'                                                   
         BNE   PRTC409                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BE    PRTC408                                                          
*                                                                               
         USING PCOMELEM,R6                                                      
PRTC408  ZIC   R1,PCOMELEM+1                                                    
         SH    R1,=H'2'                                                         
         CH    R1,=Y(L'VALSPACE)                                                
         BNH   *+14                                                             
         MVC   VALSPACE,PCOMELEM+2    SEE PPBUY05, LABEL FMTCOM                 
         B     PRTC409                                                          
*                                                                               
         CH    R1,=H'2'                                                         
         BNH   PRTC409                                                          
         XC    VALSPACE,VALSPACE                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VALSPACE(0),PCOMELEM+2                                           
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*           GOT ONE, PRINT IT OUT                                               
*-------------------------------------------------------------------            
PRTC409  GOTO1 =A(DCORR),RR=RELO                                                
         MVI   GOTCORR,C'Y'        FOUND A CORRECTION                           
*                                                                               
*        MVC   QPRD,FLD                 A BUG!!  ASTE 12/30/97                  
*                                                                               
         L     R1,CNET             CORRECTED INV NET TOTAL                      
         A     R1,ANET                                                          
         ST    R1,CNET                                                          
         L     R1,CTAX             CORRECTED INV TAX TOTAL                      
         A     R1,BUYTAX                                                        
         ST    R1,CTAX                                                          
         L     R1,CCD              CORRECTED INV CASH DISC TOTAL                
         A     R1,ACSHDSC                                                       
         ST    R1,CCD                                                           
         L     R1,CINVNUM          CORRECTED INVOICE COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,CINVNUM                                                       
*-----------------------------------------------------------------              
*        TEST FOR COMMENTS AND PRINT OUT                                        
*-----------------------------------------------------------------              
         OC    CORRCMDT,CORRCMDT   TEST FOR COMMENTS                            
         BZ    PRTC490                                                          
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'30'       COMMENT ELEMENT                              
         MVC   MINEKEY+1(L'PIMDTLS1),LSTHDRSQ   HEADER SEQ NUM                  
         MVC   MINEKEY+2(L'PIMDTLS2),CORRCMDT   DETAIL SEQ NUM                  
         BAS   RE,MINIOHI                                                       
         BNE   PRTC490                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ   COMMENT ELEMENT?                             
         BNE   PRTC490                                                          
         CLC   PIMCOMS2,CORRCMDT   SAME DETAIL SEQ NUM?                         
         BNE   PRTC490                                                          
*                                                                               
         LA    R3,PIMCOMTX         FIRST COMMENT LINE                           
*                                                                               
         CLI   PIMCOML1,0          ANY COMMENT IN LINE 1                        
         BE    CCOMM2              NO                                           
         ZIC   R1,PIMCOML1         FIRST LINE LENGTH                            
         BCTR  R1,0                1 MORE FOR EX                                
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   CCOMMM1              NO                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P2+92(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     CCOMM2                                                           
CCOMMM1  EX    R1,*+8              MAGAZINE FORMAT                              
         B     *+10                                                             
         MVC   P2+74(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     CCOMM2                                                           
*                                                                               
CCOMM2   CLI   PIMCOML2,0          ANY COMMENT IN LINE 2                        
         BE    CCOMM3                                                           
         ZIC   R1,PIMCOML2         2ND LINE LENGTH                              
         BCTR  R1,0                1 MORE FOR EX                                
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   CCOMMM2              NO                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P3+92(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     CCOMM3                                                           
CCOMMM2  EX    R1,*+8              MAGAZINE FORMAT                              
         B     *+10                                                             
         MVC   P3+74(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     CCOMM3                                                           
*                                                                               
CCOMM3   CLI   PIMCOML3,0          ANY COMMENT IN LINE 3                        
         BE    PRTC490                                                          
         MVI   P3+62,X'00'         MAKE SURE LINE 2 PRINTS                      
         ZIC   R1,PIMCOML3         THIRD LINE LENGTH                            
         BCTR  R1,0                1 MORE FOR EX                                
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   CCOMMM3              NO                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P4+92(0),0(R3)                                                   
         B     PRTC490                                                          
CCOMMM3  EX    R1,*+8              MAGAZINE FORMAT                              
         B     *+10                                                             
         MVC   P4+74(0),0(R3)                                                   
****                                                                            
PRTC490  LA    R2,L'CORRTBL(R2)    CK NEXT CORRECTION IN TABLE                  
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    PRTBPRT             PRINT OUT LINE(S)                            
         C     R2,EOTABLE                                                       
         BNL   PRTBPRT                                                          
         B     PRTC405                                                          
*-----------------------------------------------------------------              
*                      PRINT OUT LINE(S)                                        
*-----------------------------------------------------------------              
PRTBPRT  DS    0H                                                               
*                                                                               
         CLI   GOTCORR,C'Y'        FOUND A CORRECTION                           
         BE    PRTBANN             IF NOT UPDATE TOTALS                         
         L     R1,TEMPBNET         UNMATCHED BUY NET TOTAL                      
         A     R1,UMBUYNET                                                      
         ST    R1,UMBUYNET                                                      
*                                                                               
         L     R1,UMBUYCD          UNMATCHED BUY CD  TOTAL                      
         A     R1,TEMPBCD                                                       
         ST    R1,UMBUYCD                                                       
*                                                                               
         L     R1,UMBUYTAX         UNMATCHED BUY TAX TOTAL                      
         A     R1,BUYTAX                                                        
         ST    R1,UMBUYTAX                                                      
*                                                                               
         L     R1,UMBUYNUM         UNMATCHED BUY COUNT                          
         LA    R1,1(R1)                                                         
         ST    R1,UMBUYNUM                                                      
*                                                                               
         AP    UMBUYGR(8),DUMMY(8) UNMATCHED BUY GROSS TOTAL                    
PRTBANN  GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRTBNXTS            NEXT BUY                                     
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*            PRINT OUT ALL UNMATCHED INVOICE ITEMS                              
***********************************************************************         
PRTUNMAT DS    0H                                                               
*                                                                               
         GOTO1 =A(UNMAT),RR=RELO                                                
*                                                                               
         BAS   RE,BXBOT            CLOSE BOX                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
**********************************************************************          
*                    PRINT OUT TOTALS AND STUFF                                 
**********************************************************************          
         CLI   LINE,X'2D'          CHECK IF LINES>45                            
         BL    KEEPGO                                                           
         MVI   FORCEHED,C'Y'       YES, START NEW PAGE                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,BXBOT            CLOSE TOP BOX                                
*                                                                               
KEEPGO   GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P1(24),=C'******** TOTALS ********'                              
         MVC   P1+31(2),=C'##'                                                  
         MVC   P1+42(5),=C'GROSS'                                               
         MVC   P1+57(5),=C' NET '                                               
         MVC   P1+69(9),=C'CASH DISC'                                           
         MVC   P1+82(6),=C'   TAX'                                              
         MVI   P2,X'BF'                                                         
         MVC   P2+1(88),P2                                                      
*                                                                               
         CLI   AMGRSNET,C'G'                                                    
         BNE   NETTOTH                                                          
         MVC   P1+114(12),=C'GROSS TOTALS'                                      
         MVI   P2+113,X'BF'                                                     
         MVC   P2+114(12),P2+113                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     TOT10                                                            
*                                                                               
NETTOTH  MVC   P1+115(10),=C'NET TOTALS'                                        
         MVI   P2+113,X'BF'                                                     
         MVC   P2+114(12),P2+113                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
TOT10    MVC   P1(7),=C'MATCHED'                                                
         LA    R2,P1                                                            
         EDIT  (B4,MINVNUM),(3,30(R2)),ALIGN=RIGHT,ZERO=NOBLANK                 
         EDIT  (P8,MINVGR),(13,35(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES          
         EDIT  (B4,MNET),(13,50(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES            
         EDIT  (B4,MCD),(13,65(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
         EDIT  (B4,MTAX),(10,80(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES            
*                                                                               
         MVC   P2(10),=C'DISCREPANT'                                            
         LA    R2,P2                                                            
         EDIT  (B4,CINVNUM),(3,30(R2)),ALIGN=RIGHT,ZERO=NOBLANK                 
         EDIT  (P8,CRINVGR),(13,35(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
         EDIT  (B4,CNET),(13,50(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES            
         EDIT  (B4,CCD),(13,65(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
         EDIT  (B4,CTAX),(10,80(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES            
*                                                                               
         MVC   P3(15),=C'ORDERED NOT RUN'                                       
         LA    R2,P3                                                            
         EDIT  (B4,UMBUYNUM),(3,30(R2)),ALIGN=RIGHT,ZERO=NOBLANK                
         EDIT  (P8,UMBUYGR),(13,35(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
         EDIT  (B4,UMBUYNET),(13,50(R2)),2,ALIGN=RIGHT,FLOAT=$,        X        
               MINUS=YES                                                        
         EDIT  (B4,UMBUYCD),(13,65(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
         EDIT  (B4,UMBUYTAX),(10,80(R2)),2,ALIGN=RIGHT,FLOAT=$,        X        
               MINUS=YES                                                        
*                                                                               
         MVC   P4(15),=C'RUN NOT ORDERED'                                       
         LA    R2,P4                                                            
         EDIT  (B4,UMINVNUM),(3,30(R2)),ALIGN=RIGHT,ZERO=NOBLANK                
         EDIT  (P8,UMINVGR),(13,35(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
         EDIT  (B4,UMINVNET),(13,50(R2)),2,ALIGN=RIGHT,FLOAT=$,        X        
               MINUS=YES                                                        
         EDIT  (B4,UMINVCD),(13,65(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
         EDIT  (B4,UMINVTAX),(10,80(R2)),2,ALIGN=RIGHT,FLOAT=$,        X        
               MINUS=YES                                                        
*                                                                               
         LA    R2,P1                                                            
         MVC   P1+95(17),=C'INVOICE LIST    ='                                  
         EDIT  (P8,INVGRTOT),(13,113(R2)),2,ALIGN=RIGHT,FLOAT=$,       X        
               MINUS=YES                                                        
         LA    R2,P3                                                            
         MVC   P3+95(17),=C'FILE            ='                                  
         CLI   AMGRSNET,C'G'                                                    
         BNE   NETTOT                                                           
         EDIT  (P8,BUYGRTOT),(13,113(R2)),2,ALIGN=RIGHT,FLOAT=$,       X        
               MINUS=YES                                                        
         B     TOT20                                                            
NETTOT   EDIT  (B4,BUYNETOT),(13,113(R2)),2,ALIGN=RIGHT,FLOAT=$,       X        
               MINUS=YES                                                        
TOT20    DS    0H                                                               
         CLI   AMGRSNET,C'G'                                                    
         BNE   NETTOTD                                                          
         AP    INVGR(8),MINVGR(8)                                               
         AP    INVGR(8),UMINVGR(8)                                              
         AP    INVGR(8),CRINVGR(8)                                              
         LA    R2,P2                                                            
         MVC   P2+95(17),=C'INVOICE DETAILS ='                                  
         EDIT  (P8,INVGR),(13,113(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES          
         B     TOT40                                                            
NETTOTD  DS    0H                                                               
         L     R1,MNET                                                          
         A     R1,UMINVNET                                                      
         A     R1,CNET                                                          
         LA    R2,P2                                                            
         MVC   P2+95(17),=C'INVOICE DETAILS ='                                  
         EDIT  (R1),(13,113(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES                
*                                                                               
TOT40    GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    CINVNUM,CINVNUM     ANY CORRECTED INVOICES = NO MATCH            
         BNZ   TOTX                                                             
         CP    CRINVGR(8),=P'0'                                                 
         BNE   TOTX                                                             
         CP    UMBUYGR(8),=P'0'                                                 
         BNE   TOTX                                                             
         CP    UMINVGR(8),=P'0'                                                 
         BNE   TOTX                                                             
         MVC   P1+45(40),=C'****************************************'           
         MVC   P2+45(40),=C'*                                      *'           
         MVC   P3+45(40),=C'*         MATCHING SUCCESSFUL          *'           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P1+45(40),=C'*                                      *'           
         MVC   P2+45(40),=C'****************************************'           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     TOTXX                                                            
*                                                                               
TOTX     MVC   P1+45(40),=C'****************************************'           
         MVC   P2+45(40),=C'*                                      *'           
         MVC   P3+45(40),=C'*        MATCHING UNSUCCESSFUL         *'           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P1+45(40),=C'*                                      *'           
         MVC   P2+45(40),=C'****************************************'           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     TOTXX                                                            
TOTXX    MVI   FORCEHED,C'Y'       EJECT PAGE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                         GROSS TO NET                                          
***********************************************************************         
GRTONET  NTR1                                                                   
         CP    AMPERCTG,=P'0'      IF NO PERCENTAG                              
         BE    GTON10              THEN NET = GROSS                             
*                                                                               
         ZAP   AMNETAMT,=P'100000'       NET = GROSS * (1-%AGE)                 
         SP    AMNETAMT,AMPERCTG                                                
         ZAP   AMPERCTG,AMNETAMT                                                
         MP    AMGRSAMT,AMPERCTG                                                
         SRP   AMGRSAMT,64-5,5           KEEP ALL DECIMALS INTACT               
GTON10   ZAP   AMNETAMT,AMGRSAMT                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         EJECT                                                                  
******************************************************************              
*          BOX PARTS AND SPECS                                                  
******************************************************************              
BXBOT    NTR1                                                                   
         ICM   R3,15,ABOX          BOXES?                                       
         BZ    BXBX                                                             
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         MVI   BOXINIT,0                                                        
BXBX     B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
*****************************************************************               
BXTOP    NTR1                                                                   
         ICM   R3,15,ABOX          BOXES?                                       
         BZ    BXTX                                                             
         USING BOXD,R3                                                          
         L     R6,0(R1)            LHS                                          
         L     RE,4(R1)            RHS                                          
         L     R2,8(R1)            MID LINES                                    
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         LTR   R2,R2               DO WE WANT A MIDDLE LINE                     
         BZ    BXT10                                                            
         AR    RF,R2               NUMBER OF LINES TO MIDDLE LINE               
         MVI   0(RF),C'M'                                                       
*                                                                               
BXT10    LA    R6,BOXCOLS(R6)                                                   
         MVI   0(R6),C'L'          LEFT CORNER                                  
         LA    RE,BOXCOLS(RE)                                                   
         MVI   0(RE),C'R'          RIGHT CORNER                                 
*                                                                               
BXT30    CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   BXT40               NO, THEN MAGAZINE BOX                        
         BAS   RE,NBXCOL                                                        
         B     BXT50                                                            
BXT40    BAS   RE,MBXCOL                                                        
BXT50    MVI   BOXINIT,0                                                        
BXTX     B     XIT                                                              
         SPACE 2                                                                
************************************                                            
NBXCOL   NTR1                                                                   
         MVI   BOXCOLS+2,C'C'                                                   
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+18,C'C'                                                  
         MVI   BOXCOLS+22,C'C'                                                  
         MVI   BOXCOLS+31,C'C'                                                  
         MVI   BOXCOLS+46,C'C'                                                  
         MVI   BOXCOLS+58,C'C'                                                  
         MVI   BOXCOLS+64,C'C'                                                  
         MVI   BOXCOLS+76,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
*                                                                               
         B     XIT                                                              
*                                                                               
MBXCOL   NTR1                                                                   
         MVI   BOXCOLS+3,C'C'                                                   
         MVI   BOXCOLS+10,C'C'                                                  
         MVI   BOXCOLS+19,C'C'                                                  
         MVI   BOXCOLS+25,C'C'                                                  
         MVI   BOXCOLS+43,C'C'                                                  
         MVI   BOXCOLS+49,C'C'                                                  
         MVI   BOXCOLS+61,C'C'                                                  
         MVI   BOXCOLS+73,C'C'                                                  
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
******************************************************************              
*            HEAD HOOKS AND HEADSPECS (AND MIDHOOKS)                            
******************************************************************              
NHDSPECS SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,1,C'CLIENT  ='                                                
         SSPEC H4,98,RUN                                                        
         SSPEC H5,1,C'PRODUCT ='                                                
         SSPEC H5,98,REPORT                                                     
         SSPEC H5,121,PAGE                                                      
         SSPEC H6,1,C'PUB#    ='                                                
         SSPEC H7,1,C'INV#    ='                                                
         SSPEC H8,1,C'INV DATE='                                                
         SSPEC H10,1,C'******** ORDERED ********'                               
         SSPEC H13,1,C'IN ZN,EDT DATE     EST SIZE         '                    
         SSPEC H13,48,C'PREMIUM     CTPBI     GROSS  '                          
         SSPEC H13,84,C'NET   CASH DSC   BILLABLE PAYABLE PRD'                  
         SSPEC H13,122,C'INVOICE NUM'                                           
         DC    X'00'                                                            
**********                                                                      
NHDHOOK  NTR1                                                                   
         MVC   H1(10),MEDNM                                                     
         MVC   H1+51(23),=C'INVOICE MATCHING REPORT'                            
         MVI   H2+51,X'BF'                                                      
         MVC   H2+52(22),H2+51                                                  
         MVC   H3+51(22),PERIOD                                                 
         MVC   H4+10(3),QCLT                                                    
         MVC   H4+14(20),CLTNM                                                  
         MVC   H5+10(3),QPRD                                                    
         MVC   H5+14(20),PRDNM                                                  
         MVC   H6+10(14),PUBNUMH                                                
         MVC   H6+51(40),PUBINFO                                                
         MVC   H7+46(4),REPCODE                                                 
         MVC   H7+51(30),REPNAME                                                
         MVC   H8+51(30),REPADR1                                                
         MVC   H9+51(30),REPADR2                                                
         MVC   H7+10(11),INVNUMH                                                
         MVC   H7+97(8),ESTWORD                                                 
         MVC   H8+97(3),HEST                                                    
         MVC   H8+101(20),HESTNM                                                
         MVC   H9+97(17),HESTPERD                                               
         MVC   H8+10(11),INVDTH                                                 
         MVC   H13+32(10),RATEHED                                               
*                                                                               
         ICM   RF,15,ABOX          BOXES?                                       
         BZ    NHDHOOKX                                                         
*                                                                               
         USING BOXD,RF                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXROWS+11,C'T'                                                  
         MVI   BOXROWS+13,C'M'                                                  
         MVI   BOXROWS+57,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+2,C'C'                                                   
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+18,C'C'                                                  
         MVI   BOXCOLS+22,C'C'                                                  
         MVI   BOXCOLS+31,C'C'                                                  
         MVI   BOXCOLS+46,C'C'                                                  
         MVI   BOXCOLS+58,C'C'                                                  
         MVI   BOXCOLS+64,C'C'                                                  
         MVI   BOXCOLS+76,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
*        MVI   BOXCOLS+131,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
*                                                                               
NHDHOOKX B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------                
*              FOR MAGAZINES                                                    
*---------------------------------------------------------------                
MHDSPECS SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,1,C'CLIENT  ='                                                
         SSPEC H4,98,RUN                                                        
         SSPEC H5,1,C'PRODUCT ='                                                
         SSPEC H5,98,REPORT                                                     
         SSPEC H5,121,PAGE                                                      
         SSPEC H6,1,C'PUB#    ='                                                
         SSPEC H7,1,C'INV#    ='                                                
         SSPEC H8,1,C'INV DATE='                                                
         SSPEC H10,1,C'******** ORDERED ********'                               
         SSPEC H13,2,C'IN ZN,EDT DATE      EST  SPACE'                          
         SSPEC H13,45,C'CTPBI       GROSS'                                      
         SSPEC H13,69,C'NET   CASH DSC     BILLABLE   PAYABLE    PRD'           
         SSPEC H13,115,C'INVOICE NUM'                                           
         DC    X'00'                                                            
**********                                                                      
MHDHOOK  NTR1                                                                   
         MVC   H1(10),MEDNM                                                     
         MVC   H1+51(23),=C'INVOICE MATCHING REPORT'                            
         MVI   H2+51,X'BF'                                                      
         MVC   H2+52(22),H2+51                                                  
         MVC   H3+51(22),PERIOD                                                 
         MVC   H4+10(3),QCLT                                                    
         MVC   H4+14(20),CLTNM                                                  
         MVC   H5+10(3),QPRD                                                    
         MVC   H5+14(20),PRDNM                                                  
         MVC   H6+10(14),PUBNUMH                                                
         MVC   H6+51(40),PUBINFO                                                
         MVC   H7+46(4),REPCODE                                                 
         MVC   H7+51(30),REPNAME                                                
         MVC   H8+51(30),REPADR1                                                
         MVC   H9+51(30),REPADR2                                                
         MVC   H7+10(11),INVNUMH                                                
         MVC   H7+97(8),ESTWORD                                                 
         MVC   H8+97(3),HEST                                                    
         MVC   H8+101(20),HESTNM                                                
         MVC   H9+97(17),HESTPERD                                               
         MVC   H8+10(11),INVDTH                                                 
*                                                                               
         ICM   RF,15,ABOX          BOXES?                                       
         BZ    MHDHOOKX                                                         
*                                                                               
         USING BOXD,RF                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXROWS+11,C'T'                                                  
         MVI   BOXROWS+13,C'M'                                                  
         MVI   BOXROWS+57,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+3,C'C'                                                   
         MVI   BOXCOLS+10,C'C'                                                  
         MVI   BOXCOLS+19,C'C'                                                  
         MVI   BOXCOLS+25,C'C'                                                  
         MVI   BOXCOLS+43,C'C'                                                  
         MVI   BOXCOLS+49,C'C'                                                  
         MVI   BOXCOLS+61,C'C'                                                  
         MVI   BOXCOLS+73,C'C'                                                  
         MVI   BOXCOLS+129,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
*                                                                               
MHDHOOKX B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*                         ERROR MESSAGES (AND STUFF)                            
*----------------------------------------------------------------------         
RELO     DS    A                                                                
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         GOTO1 MYERR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*  THIS ROUTINE DISPLAYS THE CORRECTION DETAIL                                  
*********************************************************************           
DCORR    NTR1  BASE=*,LABEL=*                                                   
         USING CORRTBLD,R2                                                      
*-------------------------------------------------------------------            
*                   DATE (NEWS AND MAGS)                                        
*-------------------------------------------------------------------            
         CLC   CORRDATE,CORBDATE   SAME DATE                                    
         BE    PRTC410                                                          
         GOTO1 DATCON,DMCB,(3,CORRDATE),(12,PR2NIDT)  PUT OUT DATE              
*------------------------------------------------------------------             
*                       NEWSPAPER ONLY                                          
*------------------------------------------------------------------             
PRTC410  DS    0H                                                               
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   PRTC450                                                          
*------------------------------------------------------------------             
*             TEAR SHEET AND ESTIMATE (NEWS ONLY)                               
*------------------------------------------------------------------             
         LA    R3,PR2NPBC                                                       
         MVI   1(R3),C' '                                                       
         TM    CORRSTAT,X'08'      TEAR SHEET                                   
         BZ    *+8                                                              
         MVI   1(R3),C'T'                                                       
*                                                                               
         CLC   CORREST,CORBEST                                                  
         BE    PRTC411                                                          
         OC    CORREST,CORREST                                                  
         BZ    PRTC411                                                          
         EDIT  (B2,CORREST),(3,PR2NEST),FILL=0                                  
*------------------------------------------------------------------             
*                  SIZE (NEWS ONLY)                                             
*------------------------------------------------------------------             
PRTC411  LA    R3,PR2NSIZ                                                       
         CP    CORRUNIT,VALUNITS   IF SAME SIZE                                 
         BNE   PRTC412                                                          
*                                                                               
         OC    CORRCLMS,CORRCLMS                                                
         BNZ   *+10                                                             
         ZAP   CORRCLMS,=P'0'                                                   
*                                                                               
         CP    CORRCLMS,VALCLMS         AND SAME NUMBER OF COLUMNS              
         BNE   PRTC412                                                          
         CLC   CORRSPCE,VALSPACE                                                
         BE    PRTC430             THEN NO NEED TO DISPLAY IT AGAIN             
         B     PRTC413                                                          
*                                                                               
PRTC412  CLI   CORRSPCE,C' '       IF 1ST BYTE > SPACE?                         
         BNH   PRTC414                                                          
         CLI   CORRSPCE,X'FF'      3 BYTES PACKED FOLLOWS?                      
         BNE   PRTC413                                                          
         EDIT  (P3,7(R2)),(8,0(R3)),ALIGN=LEFT   YES, IT'S 12(ELEM)             
         B     PRTC430                                                          
*                                                                               
PRTC413  MVC   0(8,R3),CORRSPCE                                                 
         B     PRTC430                                                          
*                                                                               
PRTC414  CLI   CORRUIND,0           PRINT SIZE WITH NO DECIMALS?                
         BE    PRTC415                                                          
         CLI   CORRUIND,C'L'                                                    
         BE    PRTC415                                                          
         CLI   CORRUIND,C'I'                                                    
         BNE   PRTC424                                                          
*                                                                               
PRTC415  EDIT  (P3,CORRUNIT),(8,0(R3)),ALIGN=LEFT     YES                       
*                                                                               
         SR    R1,R1               THEN PUT AN 'I' AFTER THE NUMBER             
PRTC416  LA    R3,PR2NSIZ                                                       
         AR    R3,R1                                                            
         CLI   0(R3),C' '                                                       
         BE    PRTC420                                                          
         LA    R1,1(R1)                                                         
         CH    R1,=Y(L'PR2NSIZ)                                                 
         BL    PRTC416                                                          
         B     PRTC427                                                          
*                                                                               
PRTC420  CLI   CORRUIND,C'I'        IF INCHES                                   
         BE    *+12                                                             
         CLI   CORRUIND,X'89'                                                   
         BNE   PRTC427                                                          
*                                                                               
         MVI   0(R3),C'I'                                                       
         LA    R3,1(R3)                                                         
         B     PRTC427                                                          
*                                                                               
PRTC424  CLI   CORRUIND,X'89'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P3,CORRUNIT),(8,PR2NSIZ),2,ALIGN=LEFT                           
         SR    R1,R1                                                            
         B     PRTC416                                                          
*                                                                               
PRTC427  OC    CORRCLMS,CORRCLMS                                                
         BNZ   *+10                                                             
         ZAP   CORRCLMS,=P'0'                                                   
         ZAP   DUB,CORRCLMS                                                     
         BZ    PRTC430                                                          
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P8,DUB),(4,0(R3)),ALIGN=LEFT                                    
*------------------------------------------------------------------             
*                  RATE (NEWS ONLY)                                             
*------------------------------------------------------------------             
PRTC430  ZAP   P11,CORRCOST                                                     
*                                                                               
         CLI   CORRCSIN,C'S'                                                    
         BE    PRTC431                                                          
*                                                                               
         CLI   AMGRSNET,C'N'       SHOW NET AMOUNTS?                            
         BNE   PRTC431                                                          
         ZAP   AMGRSAMT,P11        YES                                          
         ZAP   AMPERCTG,MYPUBAC                                                 
         BAS   RE,GRTONET                                                       
         ZAP   P11,AMNETAMT                                                     
*                                                                               
PRTC431  DS    0H                                                               
         TM    CORRSTAT,X'80'      UNIT COST?                                   
         BZ    PRTC434             NO                                           
*                                                                               
         LA    R3,PR2NRTE                                                       
*                                                                               
         CLI   CORRCSIN,C'S'                                                    
         BNE   *+12                                                             
         MVI   PR2NRTE,C'S'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
         CP    P11,=P'-999999999'  GREATER THAN 9999.99999?                     
         BL    *+14                                                             
         CP    P11,=P'999999999'                                                
         BNH   PRTC433               NO, 5 DECIMALS                             
         ZAP   AMGRSAMT,P11                                                     
         SRP   AMGRSAMT,64-3,0                                                  
         ZAP   P11,AMGRSAMT(8)                                                  
*                                                                               
         CLI   CORRCSIN,C'S'                                                    
         BE    PRTC431B                                                         
*                                                                               
PRTC431A EDIT  (P11,P11),(12,(R3)),2,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     PRTC437                                        2 DEC             
*                                                                               
PRTC431B EDIT  (P11,P11),(11,(R3)),2,ALIGN=LEFT,MINUS=YES                       
         B     PRTC437                                                          
*                                                                               
PRTC433  CLI   CORRCSIN,C'S'                                                    
         BE    PRTC433B                                                         
*                                                                               
PRTC433A EDIT  (P11,P11),(12,(R3)),5,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     PRTC437                                        2 DEC             
*                                                                               
PRTC433B EDIT  (P11,P11),(11,(R3)),5,ALIGN=LEFT,MINUS=YES                       
         B     PRTC437                                                          
*                                                                               
PRTC434  LA    R3,PR2NRTE                                                       
         CLI   CORRCSIN,C'S'                                                    
         BNE   PRTC434A                                                         
         MVI   0(R3),C'S'                                                       
         LA    R3,1(R3)                                                         
         B     PRTC435                                                          
*                                                                               
PRTC434A MVI   PR2NRTE,C'T'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
PRTC435  EDIT  (P11,P11),(11,(R3)),2,ALIGN=LEFT,MINUS=YES                       
*------------------------------------------------------------------             
*                  PREMIUM (NEWS ONLY)                                          
*------------------------------------------------------------------             
PRTC437  CLC   CORRCLRS,VALCL       NO COLORS?                                  
         BE    PRTC440                                                          
         EDIT  (B1,CORRCLRS),(1,PR2NPRM),ZERO=NOBLANK                           
         MVI   PR2NPRM+1,C'C'                                                   
*                                                                               
PRTC440  CP    CORRPREM,VALPRCOS                                                
         BE    PRTC460                                                          
         CLC   CORRCLRS,VALCL       NO COLORS?                                  
         BE    PRTC445                                                          
         MVI   PR2NPRM+2,C'/'                                                   
         LA    R1,PR2NPRM+3                                                     
         EDIT  (P5,CORRPREM),(8,(R1)),2,ALIGN=LEFT,MINUS=YES                    
         B     PRTC460                                                          
*                                                                               
PRTC445  EDIT  (P5,CORRPREM),(11,PR2NPRM),2,ALIGN=LEFT,MINUS=YES                
         B     PRTC460                                                          
         EJECT                                                                  
*------------------------------------------------------------------             
*               SPACE, ESTIMATE- MAGAZINES ONLY                                 
*------------------------------------------------------------------             
PRTC450  CLC   CORRSPCE,VALSPACE   MAGAZINE USES SPACE                          
         BE    PRTC460                                                          
         CLI   CORRSPCE,X'FF'                                                   
         BE    PRTC452                                                          
         MVC   PR2MSPC,CORRSPCE                                                 
         B     PRTC460                                                          
*                                                                               
PRTC452  LA    R3,PR2MSPC                                                       
         MVC   0(4,R3),=C'SRI='    DISPLAY  SHOW/REG/ILLUM                      
         LA    R3,4(R3)                                                         
         CP    CORRSHOW,=P'99999'                                               
         BNE   PRTC452A                                                         
         MVC   0(3,R3),=C'SPC'                                                  
         LA    R3,3(R3)                                                         
         B     PRTC452B                                                         
PRTC452A EDIT  (P3,CORRSHOW),(3,0(R3)),ALIGN=LEFT                               
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
PRTC452B MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P3,CORRREG),(4,0(R3)),ALIGN=LEFT                                
         LA    R3,3(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P3,CORILLUM),(4,0(R3)),ALIGN=LEFT                               
*                                                                               
         CLC   CORREST,CORBEST                                                  
         BE    PRTC460                                                          
         OC    CORREST,CORREST                                                  
         BZ    PRTC460                                                          
         EDIT  (B2,CORREST),(3,PR2MEST),FILL=0                                  
*------------------------------------------------------------------             
*              BOTH NEWS AND MAGS                                               
*------------------------------------------------------------------             
PRTC460  LA    R3,PR2NPBC                                                       
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PR2MPBC                                                       
*                                                                               
         MVI   0(R3),C'N'                                                       
         TM    CORRSTAT,X'40'      CASH DISC                                    
         BNZ   *+8                                                              
         MVI   0(R3),C'Y'                                                       
*                                                                               
         MVI   1(R3),C' '                                                       
         TM    CORRSTAT,X'08'      TEAR SHEET                                   
         BZ    *+8                                                              
         MVI   1(R3),C'T'                                                       
*------------------------------------------------------------------             
*          GROSS, NET AND CASH DISCOUNT (BOTH)                                  
*------------------------------------------------------------------             
*                                  CALCDTLG NEED MINELEM                        
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         MVC   PIMUIND,CORRUIND                                                 
         MVC   PIMUNITS,CORRUNIT                                                
         MVC   PIMCLMS,CORRCLMS                                                 
         MVC   PIMCOST,CORRCOST                                                 
         MVC   PIMPREM,CORRPREM                                                 
         MVC   PIMCLRS,CORRCLRS                                                 
         MVC   PIMDSTAT,CORRSTAT                                                
         MVC   PIMCSIND,CORRCSIN                                                
*                                  GROSS, NET, AND CASH DISCOUNT                
         XC    ANET,ANET                                                        
         XC    BUYTAX,BUYTAX                                                    
         XC    ACSHDSC,ACSHDSC                                                  
         GOTO1 CALCDTLG,DMCB,GETINSA                                            
         LA    R1,GETINSA                                                       
         USING PVALUES,R1                                                       
*                                                                               
         MVC   BUYTAX,TAX                                                       
         MVC   FULL,GROSS          GROSS                                        
         LA    R3,PR2NGRS                                                       
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PR2MGRS                                                       
         EDIT  (B4,FULL),(11,0(R3)),FILL=0                                      
         PACK  WKSPACE(8),0(11,R3)                                              
         AP    CRINVGR(8),WKSPACE(8)                                            
         EDIT  (B4,FULL),(11,0(R3)),2,ALIGN=RIGHT,MINUS=YES                     
*                                                                               
         CLI   CORRCSIN,C'S'                                                    
         BNE   *+10                                                             
         XC    AGYCOM,AGYCOM                                                    
*                                                                               
         LA    R3,PR2NCD                                                        
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PR2MCD                                                        
         TM    PIMDSTAT,X'40'      CASH DISC                                    
         BNZ   *+14                                                             
         OC    CSHDSC,CSHDSC                                                    
         BNZ   PRTC462                                                          
*                                                                               
         MVC   3(4,R3),=C'NONE'                                                 
         B     PRTC464                                                          
*                                                                               
PRTC462  DS    0H                                                               
         MVC   ACSHDSC,CSHDSC                                                   
         EDIT  (B4,CSHDSC),(8,0(R3)),2,ALIGN=LEFT,MINUS=YES                     
*                                                                               
PRTC464  DS    0H                  NET                                          
         L     R3,GROSS                                                         
         L     R0,AGYCOM                                                        
         SR    R3,R0                                                            
         ST    R3,FULL                                                          
         ST    R3,ANET                                                          
         LA    R3,PR2NNET                                                       
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PR2MNET                                                       
         EDIT  (B4,FULL),(11,0(R3)),2,ALIGN=RIGHT,MINUS=YES                     
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* ROUTINE FOR PRINTING UNMATCHED ITEMS                                          
********************************************************************            
UNMAT    NTR1  BASE=*,LABEL=*                                                   
*----------------------------------------------------------------               
*        CLOSE TOP BOX AND SET UP BOTTOM BOX                                    
*----------------------------------------------------------------               
         BAS   RE,BXBOT                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   LINE,X'32'          CHECK IF LINES>50                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'       YES, START NEW PAGE                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P1(33),=C'******** RUN NOT ORDERED ********'                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   MAGTOP                                                           
         GOTO1 BXTOP,DMCB,0,131,2                                               
         B     PRINTIT                                                          
*                                                                               
MAGTOP   GOTO1 BXTOP,DMCB,0,131,2                                               
*                                                                               
PRINTIT  GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   MAGHEADS                                                         
         MVC   RATEHED,=C'GROSS RATE'                                           
         CLI   AMGRSNET,C'N'                                                    
         BNE   *+10                                                             
         MVC   RATEHED,=C'NET RATE  '                                           
         MVC   P(39),=C'IN ZN,EDT DATE     EST SIZE         '                   
         MVC   P+32(10),RATEHED                                                 
         MVC   P+47(29),=C'PREMIUM     CTPBI     GROSS  '                       
         MVC   P+83(25),=C'NET   CASH DSC / COMMENTS'                           
         MVC   P+117(3),=C'PRD'                                                 
         B     SOMEWH                                                           
*                                                                               
MAGHEADS MVC   P(31),=C' IN ZN,EDT DATE      EST  SPACE'                        
         MVC   P+44(17),=C'CTPBI       GROSS'                                   
         MVC   P+68(25),=C'NET   CASH DSC / COMMENTS'                           
         MVC   P+109(3),=C'PRD'                                                 
*                                                                               
SOMEWH   DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*----------------------------------------------------------------               
*        GET DETAILS  FROM MINIO                                                
*----------------------------------------------------------------               
PRTU500  DS    0H                                                               
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   PRTUX                                                            
         B     PRTU507                                                          
*                                                                               
PRTU505H DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'MINEKSV),MINEKSV     RESTORE X'20' SEQUENCE            
         BAS   RE,MINIOHI                                                       
         BNE   PRTUX                                                            
PRTU505S BAS   RE,MINIOSEQ         GET NEXT MINIO ELEMENT                       
         BNE   PRTUX                                                            
         MVC   MINEKSV,MINEKEY                                                  
*                                                                               
PRTU507  L     R6,MINELEM          DETAIL ELEMENT STILL?                        
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   PRTUX                                                            
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ   SAME HEADER SEQ NUM?                         
         BNE   PRTUX                                                            
*                                                                               
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2                                   
         MVC   MINEKSV,MINEKEY     TO RESTORE X'20' SEQUENCE                    
*                                                                               
         CLC   PIMIDATE,INVSTDT   WITHIN PERIOD?                                
         BL    PRTU505S           NO, NEXT                                      
         CLC   PIMIDATE,INVENDDT                                                
         BH    PRTU505S                                                         
*                                                                               
         CLI   PIMBLINE,0          IF THERE IS A BUYLINE                        
         BE    PRTU510                                                          
*                                                                               
         B     PRTU505S            NO,NEXT                                      
*----------------------------------------------------------------               
*        ZONE, EDT (BOTH NEWS AND MAGS)                                         
*----------------------------------------------------------------               
PRTU510  DS    0H                                                               
         MVC   FAKEPUB,BPUB                                                     
         MVC   FAKEPUB+4(1),PIMBZONE                                            
         MVC   FAKEPUB+5(1),PIMBEDTN                                            
         XC    PUBEXP,PUBEXP                                                    
         GOTO1 VPUBEDIT,DMCB,(8,FAKEPUB),(C'S',PUBEXP)                          
         MVC   PRTNZE,PUBEXP+9                                                  
*                                                                               
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   PRTU550                                                          
*----------------------------------------------------------------               
*            DATE, ESTIMATE (NEWSPAPERS ONLY)                                   
*----------------------------------------------------------------               
         GOTO1 DATCON,DMCB,(3,PIMIDATE),(12,PRTNIDT)                            
         CLI   MAGFREQ,C'M'        MONTHLY?                                     
         BNE   PRTU510A                                                         
         MVC   PRTNIDT+3(2),=C'  '       BLANK OUT DAY                          
PRTU510A OC    PIMIEST,PIMIEST                                                  
         BZ    PRTU511                                                          
         EDIT  (B2,PIMIEST),(3,PRTNEST),FILL=0                                  
*----------------------------------------------------------------               
*            SIZE (NEWSPAPERS ONLY)                                             
*----------------------------------------------------------------               
PRTU511  LA    R3,PRTNSIZ                                                       
*                                                                               
         OC    PIMCLMS,PIMCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PIMCLMS,=P'0'                                                    
*                                                                               
PRTU512  CLI   PIMSPACE,C' '       IF 1ST BYTE > SPACE?                         
         BNH   PRTU514                                                          
         CLI   PIMSPACE,X'FF'      3 BYTES PACKED FOLLOWS?                      
         BNE   PRTU513                                                          
         EDIT  (P3,12(R6)),(8,0(R3)),ALIGN=LEFT   YES, IT'S 12(ELEM)            
         B     PRTU530                                                          
*                                                                               
PRTU513  MVC   0(8,R3),PIMSPACE                                                 
         B     PRTU530                                                          
*                                                                               
PRTU514  CLI   PIMUIND,0           PRINT SIZE WITH NO DECIMALS?                 
         BE    PRTU515                                                          
         CLI   PIMUIND,C'L'                                                     
         BE    PRTU515                                                          
         CLI   PIMUIND,C'I'                                                     
         BNE   PRTU524                                                          
*                                                                               
PRTU515  EDIT  (P3,PIMUNITS),(8,0(R3)),ALIGN=LEFT     YES                       
*                                                                               
         SR    R1,R1               THEN PUT AN 'I' AFTER THE NUMBER             
PRTU516  LA    R3,PRTNSIZ                                                       
         AR    R3,R1                                                            
         CLI   0(R3),C' '                                                       
         BE    PRTU520                                                          
         LA    R1,1(R1)                                                         
         CH    R1,=Y(L'PRTNSIZ)                                                 
         BL    PRTU516                                                          
         B     PRTU527                                                          
*                                                                               
PRTU520  CLI   PIMUIND,C'I'        IF INCHES                                    
         BE    *+12                                                             
         CLI   PIMUIND,X'89'                                                    
         BNE   PRTU527                                                          
*                                                                               
         MVI   0(R3),C'I'                                                       
         LA    R3,1(R3)                                                         
         B     PRTU527                                                          
*                                                                               
PRTU524  CLI   PIMUIND,X'89'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P3,PIMUNITS),(8,PRTNSIZ),2,ALIGN=LEFT                           
         SR    R1,R1                                                            
         B     PRTU516                                                          
*                                                                               
PRTU527  OC    PIMCLMS,PIMCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PIMCLMS,=P'0'                                                    
         ZAP   DUB,PIMCLMS                                                      
         BZ    PRTU530                                                          
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P8,DUB),(4,0(R3)),ALIGN=LEFT                                    
*----------------------------------------------------------------               
*            RATE (NEWSPAPERS ONLY)                                             
*----------------------------------------------------------------               
PRTU530  DS    0H                                                               
*                                                                               
         ZAP   P11,PIMCOST                                                      
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BE    PRTU531                                                          
*                                                                               
         CLI   AMGRSNET,C'N'       SHOW NET AMOUNTS?                            
         BNE   PRTU531                                                          
         ZAP   AMGRSAMT,P11        YES                                          
         ZAP   AMPERCTG,MYPUBAC                                                 
         BAS   RE,GRTONET                                                       
         ZAP   P11,AMNETAMT                                                     
*                                                                               
PRTU531  DS    0H                                                               
         TM    PIMDSTAT,X'80'      UNIT COST?                                   
         BZ    PRTU534             NO                                           
*                                                                               
         LA    R3,PRTNRTE                                                       
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BNE   *+12                                                             
         MVI   PRTNRTE,C'S'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
         CP    P11,=P'-999999999'  GREATER THAN 9999.99999?                     
         BL    *+14                                                             
         CP    P11,=P'999999999'                                                
         BNH   PRTU533               NO, 5 DECIMALS                             
         ZAP   AMGRSAMT,P11                                                     
         SRP   AMGRSAMT,64-3,0                                                  
         ZAP   P11,AMGRSAMT(8)                                                  
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BE     PRTU531B                                                        
*                                                                               
PRTU531A EDIT  (P11,P11),(12,(R3)),2,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     PRTU537                                        2 DEC             
*                                                                               
PRTU531B EDIT  (P11,P11),(11,(R3)),2,ALIGN=LEFT,MINUS=YES                       
         B     PRTU537                                                          
*                                                                               
PRTU533  CLI   PIMCSIND,C'S'                                                    
         BE     PRTU533B                                                        
*                                                                               
PRTU533A EDIT  (P11,P11),(12,(R3)),5,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     PRTU537                                        2 DEC             
*                                                                               
PRTU533B EDIT  (P11,P11),(11,(R3)),5,ALIGN=LEFT,MINUS=YES                       
         B     PRTU537                                                          
*                                                                               
PRTU534  LA    R3,PRTNRTE                                                       
         CLI   PIMCSIND,C'S'                                                    
         BNE   PRTU534A                                                         
         MVI   0(R3),C'S'                                                       
         LA    R3,1(R3)                                                         
         B     PRTU535                                                          
*                                                                               
PRTU534A MVI   PRTNRTE,C'T'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
PRTU535  EDIT  (P11,P11),(11,(R3)),2,ALIGN=LEFT,MINUS=YES                       
*----------------------------------------------------------------               
*            PREMIUM (NEWSPAPERS ONLY)                                          
*----------------------------------------------------------------               
PRTU537  DS    0H                                                               
         LA    R1,PRTNPRM                                                       
         CLI   PIMCLRS,0                                                        
         BE    PRTU540                                                          
         EDIT  (B1,PIMCLRS),(1,PRTNPRM),ZERO=NOBLANK                            
         MVI   PRTNPRM+1,C'C'                                                   
         LA    R1,2(R1)                                                         
*                                                                               
PRTU540  CP    PIMPREM,=P'0'                                                    
         BE    PRTU560                                                          
         CLI   PIMCLRS,0                                                        
         BE    PRTU545                                                          
         MVI   PRTNPRM+2,C'/'                                                   
         LA    R1,1(R1)                                                         
         EDIT  (P5,PIMPREM),(8,(R1)),2,ALIGN=LEFT,MINUS=YES                     
         B     PRTU560                                                          
*                                                                               
PRTU545  EDIT  (P5,PIMPREM),(11,PRTNPRM),2,ALIGN=LEFT,MINUS=YES                 
         B     PRTU560                                                          
*----------------------------------------------------------------               
*            DATE, ESTIMATE,SPACE (MAGAZINES ONLY)                              
*----------------------------------------------------------------               
PRTU550  GOTO1 DATCON,DMCB,(3,PIMIDATE),(12,PRTMIDT)                            
         CLI   MAGFREQ,C'M'        MONTHLY?                                     
         BNE   PRTU551                                                          
         MVC   PRTNIDT+3(2),=C'  '       BLANK OUT DAY                          
*                                                                               
PRTU551  EDIT  (B2,PIMIEST),(3,PRTMEST),FILL=0                                  
*                                                                               
         CLI   PIMSPACE,X'FF'                                                   
         BE    PRTU552                                                          
         MVC   PRTMSPC,PIMSPACE                                                 
         B     PRTU560                                                          
*                                                                               
PRTU552  LA    R3,PRTMSPC                                                       
         MVC   0(4,R3),=C'SRI='  DISPLAY SHOW/REG/ILLUM                         
         LA    R3,4(R3)                                                         
         CP    PIMSHOW,=P'99999'                                                
         BNE   PRTU552A                                                         
         MVC   0(3,R3),=C'SPC'                                                  
         LA    R3,3(R3)                                                         
         B     PRTU552B                                                         
PRTU552A EDIT  (P3,PIMSHOW),(3,0(R3)),ALIGN=LEFT                                
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
PRTU552B MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P3,PIMREG),(4,0(R3)),ALIGN=LEFT                                 
         LA    R3,3(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         EDIT  (P3,PIMILLUM),(4,0(R3)),ALIGN=LEFT                               
         DROP  R2                                                               
*----------------------------------------------------------------               
*           CASH DISCOUNT, TEAR SHEET (BOTH)                                    
*----------------------------------------------------------------               
PRTU560  LA    R3,PRTNPBC                                                       
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PRTMPBC                                                       
*                                                                               
         MVI   0(R3),C'N'                                                       
         TM    PIMDSTAT,X'40'      CASH DISC                                    
         BNZ   *+8                                                              
         MVI   0(R3),C'Y'                                                       
*                                                                               
         MVI   1(R3),C' '                                                       
         TM    PIMDSTAT,X'08'      TEAR SHEET                                   
         BZ    *+8                                                              
         MVI   1(R3),C'T'                                                       
*----------------------------------------------------------------               
*           GROSS, NET AND CASH DISC  AND PRD(BOTH)                             
*----------------------------------------------------------------               
         CLI   QMED,C'N'                                                        
         BNE   *+14                                                             
         MVC   PRTNPRD,PIMSPRD                                                  
         B     *+10                                                             
         MVC   PRTMPRD,PIMSPRD                                                  
*                                                                               
         XC    ANET,ANET                                                        
         XC    BUYTAX,BUYTAX                                                    
         XC    ACSHDSC,ACSHDSC                                                  
         GOTO1 CALCDTLG,DMCB,GETINSA                                            
         LA    R1,GETINSA                                                       
         USING PVALUES,R1                                                       
*                                                                               
         MVC   BUYTAX,TAX                                                       
         MVC   FULL,GROSS          GROSS                                        
         LA    R3,PRTNGRS                                                       
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PRTMGRS                                                       
         EDIT  (B4,FULL),(11,0(R3)),FILL=0                                      
         PACK  WKSPACE(8),0(11,R3)                                              
         AP    UMINVGR(8),WKSPACE(8)                                            
         EDIT  (B4,FULL),(11,0(R3)),2,ALIGN=RIGHT,MINUS=YES                     
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BNE   *+10                                                             
         XC    AGYCOM,AGYCOM                                                    
*                                                                               
         TM    PIMDSTAT,X'40'      CASH DISC                                    
         BNZ   *+14                                                             
         OC    CSHDSC,CSHDSC                                                    
         BNZ   PRTU562                                                          
*                                                                               
         LA    R3,PRTNCD                                                        
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PRTMCD                                                        
         MVC   3(4,R3),=C'NONE'                                                 
         B     PRTU564                                                          
*                                                                               
PRTU562  DS    0H                                                               
         MVC   ACSHDSC,CSHDSC                                                   
         LA    R3,PRTNCD                                                        
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PRTMCD                                                        
         EDIT  (B4,CSHDSC),(8,0(R3)),2,ALIGN=RIGHT,MINUS=YES                    
*                                                                               
PRTU564  DS    0H                  NET                                          
         L     R3,GROSS                                                         
         L     R0,AGYCOM                                                        
         SR    R3,R0                                                            
         ST    R3,FULL                                                          
         ST    R3,ANET                                                          
         LA    R3,PRTNNET                                                       
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BE    *+8                                                              
         LA    R3,PRTMNET                                                       
         EDIT  (B4,FULL),(11,0(R3)),2,ALIGN=RIGHT,MINUS=YES                     
*-------------------------------------------------------------------            
*             UNMATCHED DETAILS TOTALS                                          
*-------------------------------------------------------------------            
         L     R1,UMINVNET         UNMATCHED INV NET TOTAL                      
         A     R1,ANET                                                          
         ST    R1,UMINVNET                                                      
         L     R1,UMINVCD          UNMATCHED INV CASH DISC TOTAL                
         A     R1,ACSHDSC                                                       
         ST    R1,UMINVCD                                                       
         L     R1,UMINVTAX         UNMATCHED INV TAX TOTAL                      
         A     R1,BUYTAX                                                        
         ST    R1,UMINVTAX                                                      
         L     R1,UMINVNUM         UNMATCHED INVOICE COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,UMINVNUM                                                      
*-------------------------------------------------------------------            
*             CHECK FOR COMMENTS AND PRINT                                      
*-------------------------------------------------------------------            
         TM    PIMDSTAT,X'04'      TEST FOR COMMENTS                            
         BZ    PRTUSPS                                                          
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'30'       COMMENT ELEMENT                              
         MVC   MINEKEY+1(L'PIMDTLS1),PIMDTLS1   HEADER SEQ NUM                  
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2   DETAIL SEQ NUM                  
         MVC   DTLSEQN,PIMDTLS2                                                 
         BAS   RE,MINIOHI                                                       
         BNE   PRTUSPH                                                          
         B     PRTU575                                                          
*                                                                               
PRTU575  L     R6,MINELEM                                                       
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ   COMMENT ELEMENT?                             
         BNE   PRTUSPH                                                          
         CLC   PIMCOMS2,DTLSEQN    SAME DETAIL SEQ NUM?                         
         BNE   PRTUSPH                                                          
*                                                                               
         LA    R3,PIMCOMTX         FIRST COMMENT LINE                           
*                                                                               
         CLI   PIMCOML1,0          ANY COMMENT IN LINE 1                        
         BE    COMM2                                                            
         ZIC   R1,PIMCOML1         FIRST LINE LENGTH                            
         BCTR  R1,0                1 MORE FOR EX                                
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   COMMM1              NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P2+92(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     COMM2                                                            
COMMM1   EX    R1,*+8              MAGAZINE FORMAT                              
         B     *+10                                                             
         MVC   P2+74(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     COMM2                                                            
*                                                                               
COMM2    CLI   PIMCOML2,0          ANY COMMENT IN LINE 2                        
         BE    COMM3                                                            
         ZIC   R1,PIMCOML2         2ND LINE LENGTH                              
         BCTR  R1,0                1 MORE FOR EX                                
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   COMMM2              NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P3+92(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     COMM3                                                            
COMMM2   EX    R1,*+8              MAGAZINE FORMAT                              
         B     *+10                                                             
         MVC   P3+74(0),0(R3)                                                   
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         B     COMM3                                                            
*                                                                               
COMM3    CLI   PIMCOML3,0          ANY COMMENT IN LINE 3                        
         BE    PRTUSPH                                                          
         MVI   P2+62,X'00'         MAKE SURE LINE 2 PRINTS                      
         ZIC   R1,PIMCOML3         THIRD LINE LENGTH                            
         BCTR  R1,0                1 MORE FOR EX                                
         CLI   MEDTYPE,C'N'           NEWSPAPERS?                               
         BNE   COMMM3              NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P4+92(0),0(R3)                                                   
         B     PRTUSPH                                                          
COMMM3   EX    R1,*+8              MAGAZINE FORMAT                              
         B     *+10                                                             
         MVC   P4+74(0),0(R3)                                                   
         B     PRTUSPH                                                          
*                                                                               
PRTUSPS  GOTO1 SPOOL,DMCB,(R8)     PRINT OUT LINE(S)                            
         B     PRTU505S                                                         
PRTUSPH  GOTO1 SPOOL,DMCB,(R8)     PRINT OUT LINE(S)                            
         B     PRTU505H                                                         
****                                                                            
PRTUX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*               INCLUDES                                                        
******************************************************************              
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* SCREENS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEBD          (OUR CHECK SCREEN FOR NEWSPAPER)             
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDBD          (OUR CHECK SCREEN FOR MAGAZINE)              
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDED          (OUR CHECK SCREEN FOR REPORTS)               
         EJECT                                                                  
* DDGENTWA                                                                      
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
       ++INCLUDE PPMATWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE BUYDSECTS                                                      
         EJECT                                                                  
       ++INCLUDE PUBGENEL                                                       
         EJECT                                                                  
       ++INCLUDE PPMATPRTD                                                      
         EJECT                                                                  
*********************                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079PPMAT07   01/02/06'                                      
         END                                                                    
