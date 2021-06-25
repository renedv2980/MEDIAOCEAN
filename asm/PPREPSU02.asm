*          DATA SET PPREPSU02  AT LEVEL 066 AS OF 07/18/18                      
*PHASE PPSU02A,+0,NOAUTO                                                        
*INCLUDE PPFRGTAB                                                               
*INCLUDE PPSPCVAL                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE XSORT                                                                  
         TITLE 'PPSU02 - PRINTPAK SPACE USAGE REPORT'                           
*********************************************************************           
* USER    JIRA       DATE                  CHANGE LOG                           
* ---- ----------  -------- -----------------------------------------           
* SMUR SPEC-11729  05/16/18 CHANGES FOR MEDIA D (18.3)                          
*                                                                               
*********************************************************************           
*                                                                               
* BPLA 08/15    CHANGES FOR MEDIA B, V, AND W                                   
*                                                                               
* BPLA 05/14    CHANGES FOR MEDIA L                                             
*                                                                               
* KWAN 06/00    ALLOW ONE MEDIA, ONE CLT REQUEST                                
*               SKIP BUY RECS WITH BRAND PRD OR POOL BUYS                       
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*      QOPT4     Y= CLIENT'S REPORT                                             
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPSU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPSU02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
*                                                                               
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'         BUYS READ                                    
         ZAP   SPACNT,=P'0'        SPACE DESCRIPTION ELEMS COUNTER              
         ZAP   ERRSCNT,=P'0'       ERROR FOUND IN SPACE DESCRIPTION             
         ZAP   USPACNT,=P'0'       UNIQUE SPACE DESP COUNTER                    
         ZAP   MEDMCNT,=P'0'       MEDIA M COUNTER                              
         ZAP   MEDSCNT,=P'0'       MEDIA S COUNTER                              
         ZAP   MEDTCNT,=P'0'       MEDIA T COUNTER                              
         ZAP   MEDICNT,=P'0'       MEDIA I COUNTER                              
         ZAP   MEDNCNT,=P'0'       MEDIA N COUNTER                              
         ZAP   MEDLCNT,=P'0'       MEDIA L COUNTER                              
         ZAP   MEDBCNT,=P'0'       MEDIA B COUNTER                              
         ZAP   MEDVCNT,=P'0'       MEDIA V COUNTER                              
         ZAP   MEDWCNT,=P'0'       MEDIA W COUNTER                              
         ZAP   MEDDCNT,=P'0'       MEDIA D COUNTER                              
         ZAP   SKIPCNT,=P'0'       BUY RECS SKIPPED                             
*                                                                               
         MVC   VFRGTAB,=V(PPFRGTAB)                                             
         BAS   RE,INITTAB          INITIALIZE ALL TABLES                        
*                                                                               
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGENCY/MEDIA                        
         MVI   KEY+3,X'20'         BUYS RECORD                                  
         MVC   KEY+4(3),QCLIENT    CLIENT                                       
         CLC   QCLIENT,SPACES      CHK IF DOING ALL CLIENTS                     
         BE    *+14                                                             
         CLC   QCLIENT,=C'ALL'     CHK IF DOING ALL CLIENTS                     
         BNE   *+10                                                             
         XC    KEY+4(3),KEY+4      CLEAR OUT CLIENT CODE                        
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    DS    0H                                                               
         OC    KEY+21(3),KEY+21    NO POOL BUYS OR BRAND PRDS                   
         BNZ   PROC3                                                            
         CLC   QCLIENT,SPACES      SEE IF DOING ALL CLIENTS                     
         BE    PROC7H                                                           
         CLC   QCLIENT,=C'ALL'     SEE IF DOING ALL CLIENTS                     
         BE    PROC7H                                                           
         CLC   KEY(7),KEYSAVE      AGY/MEDIA/RECORD CODE/CLIENT?                
         BNE   PROC99X             END OF REQUEST                               
         B     PROC7M                                                           
PROC7H   CLC   KEY(4),KEYSAVE      AGY/MEDIA RECORD CODE?                       
         BNE   PROC99X             END OF REQUEST                               
*                                                                               
PROC7M   AP    INCNT,=P'1'                                                      
*                                                                               
         CLI   KEY+2,C'M'          MEDIA M?                                     
         BNE   *+14                                                             
         AP    MEDMCNT,=P'1'                                                    
         B     PROC8                                                            
         CLI   KEY+2,C'S'          MEDIA S?                                     
         BNE   *+14                                                             
         AP    MEDSCNT,=P'1'                                                    
         B     PROC8                                                            
         CLI   KEY+2,C'T'          MEDIA T?                                     
         BNE   *+14                                                             
         AP    MEDTCNT,=P'1'                                                    
         B     PROC8                                                            
         CLI   KEY+2,C'I'          MEDIA I?                                     
         BNE   *+14                                                             
         AP    MEDICNT,=P'1'                                                    
         B     PROC8                                                            
         CLI   KEY+2,C'N'          MEDIA N?                                     
         BNE   *+14                                                             
         AP    MEDNCNT,=P'1'                                                    
         B     PROC8                                                            
         CLI   KEY+2,C'L'          MEDIA L?                                     
         BNE   *+14                                                             
         AP    MEDLCNT,=P'1'                                                    
         B     PROC8                                                            
*                                                                               
         CLI   KEY+2,C'B'          MEDIA B?                                     
         BNE   *+14                                                             
         AP    MEDBCNT,=P'1'                                                    
         B     PROC8                                                            
*                                                                               
         CLI   KEY+2,C'D'          MEDIA D?                                     
         BNE   *+14                                                             
         AP    MEDDCNT,=P'1'                                                    
         B     PROC8                                                            
*                                                                               
         CLI   KEY+2,C'V'          MEDIA V?                                     
         BNE   *+14                                                             
         AP    MEDVCNT,=P'1'                                                    
         B     PROC8                                                            
*                                                                               
         CLI   KEY+2,C'W'          MEDIA W?                                     
         BNE   *+14                                                             
         AP    MEDWCNT,=P'1'                                                    
         B     PROC8                                                            
*                                                                               
         AP    SKIPCNT,=P'1'       MEDIA IS NOT M,S,T,I,N,L,B,V,W,D             
         B     PROC3               NEXT BUY REC                                 
*                                                                               
PROC8    DS    0H                                                               
         GOTO1 GETPRT              GET BUY REC                                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROC10   DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'         BUY DESCRIPTION ELEM                         
         BE    PROC20                                                           
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC20   DS    0H                                                               
         AP    SPACNT,=P'1'        SPACE DESCRIPTION ELEM COUNTER               
         USING PBDELEM,R2                                                       
         MVC   WORK(L'PBDSPACE),PBDSPACE                                        
         DROP  R2                                                               
*                                                                               
         MVI   ERRSPSW,0           INITIALIZE ERROR SPACE SWITCH                
*                                                                               
         GOTO1 =V(PPSPCVAL),DMCB,(17,WORK),WKAREA,VFRGTAB                       
         CLI   12(R1),0                                                         
         BE    PROC27              NO ERROR FROM PPSPCVAL                       
         AP    ERRSCNT,=P'1'                                                    
         MVI   ERRSPSW,C'Y'        FLAG FOR ERROR SPACE DESP                    
PROC27   CP    USPACNT,=P'6000'                                                 
         BNH   *+6                                                              
         DC    H'0'                ONLY STORES 6000 UNIQUE ENTRIES              
         L     RE,ASPADTAB                                                      
PROC30   CLC   0(17,RE),WORK       SEE IF ALREADY IN TABLE                      
         BE    PROC35              NEXT SEQ                                     
         OC    0(17,RE),0(RE)      SEE IF BLANK ENTRY                           
         BZ    PROC32                                                           
         LA    RE,21(RE)           NEXT ENTRY                                   
         B     PROC30                                                           
*                                                                               
PROC32   MVC   0(17,RE),WORK       PUT INCORRECT SPACE DESP IN TABLE            
         AP    USPACNT,=P'1'                                                    
*                                                                               
PROC35   ICM   RF,15,17(RE)                                                     
         AHI   RF,1                                                             
         STCM  RF,15,17(RE)                                                     
*                                                                               
         CLI   ERRSPSW,C'Y'                                                     
         BE    PROC3               DON'T PUT THIS REC IN BIN TABLE              
*                                                                               
PROC40   DS    0H                                                               
         MVC   WORK+17(15),WKAREA                                               
         XC    WORK+32(4),WORK+32                                               
*                                                                               
         MVC   WKAREA(36),WORK     JUST IN CASE WORK GET MESS UP                
*                                                                               
         GOTO1 =V(BINSRCH),BINPARMS,(X'00',WKAREA)                              
         CLI   0(R1),X'01'                                                      
         BE    PROC50              NOT IN BIN TABLE, ADD IT                     
*                                                                               
         ZICM  R3,1(R1),(7)        POINT R3 TO TABLE - A(FOUND RECORD)          
*                                                                               
         ICM   RE,15,32(R3)        GET USAGE COUNT                              
         AHI   RE,1                                                             
         STCM  RE,15,WORK+32       INCREMENTED USAGE COUNT                      
         MVC   WKAREA(36),WORK     JUST IN CASE WORK GET MESS UP                
*                                                                               
         GOTO1 =V(BINSRCH),BINPARMS,(X'80',WKAREA)     DELETE RECORD            
         B     PROC55                                                           
*                                                                               
PROC50   MVI   WORK+35,X'01'       NOW CONTAIN 1 USAGE                          
*                                                                               
PROC55   GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)       ADD RECORD               
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         B     PROC3               NEXT SEQ                                     
*                                                                               
PROC99X  DS    0H                                                               
         BAS   RE,PRTREC           PRINT CURRENT REQ'S RESULT                   
         BAS   RE,INITTAB          REINITIALIZE TABLES AGAIN                    
         B     EXIT                NEXT REQUEST CARD                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS           PRINTING COUNTERS                            
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL80                                                           
         MVC   P(CNTLNQ-08),8(R4)                                               
         OI    7(R4),X'0F'                                                      
         UNPK  P+CNTLNQ+05(10),0(8,R4)                                          
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT             SKIP A LINE                                  
         LA    R4,CNTLNQ(R4)                                                    
         B     RUNL50                                                           
*                                                                               
RUNL80   DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPRT     NTR1                                                                   
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRTREC   NTR1                                                                   
*                                                                               
         CLI   QOPT4,C'Y'          CHK IF DOING CLIENT'S REPORT                 
         BE    PRTR50              YES                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,10                                                      
*                                                                               
         MVI   WORK,X'FF'                                                       
         MVC   WORK+1(BINKEYLQ-1),WORK                                          
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         XC    WORK(BINKEYLQ),WORK                                              
         GOTO1 =V(BINSRCH),BINPARMS,(X'02',WORK)                                
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
         XC    BTABCNT,BTABCNT     COUNTING NUMBER OF ENTRIES IN BINTAB         
         ZICM  R3,1(R1),(7)        POINT R3 TO TABLE - A(FOUND RECORD)          
         USING BTABRECD,R3                                                      
*                                                                               
PRTR20   CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    PRTR45              YES - FINISH UP                              
*                                                                               
         L     RE,BTABCNT                                                       
         AHI   RE,1                                                             
         ST    RE,BTABCNT                                                       
*                                                                               
         MVC   P+05(L'BTKSPA),BTKSPA                                            
         EDIT  (B4,BTKUSCNT),(10,P+026),ZERO=NOBLANK,ALIGN=LEFT,       +        
               COMMAS=YES                                                       
*                                                                               
         EDIT  (B4,BTKBINV1),(08,P+037),5,ZERO=NOBLANK,ALIGN=LEFT               
         EDIT  (B4,BTKBINV2),(08,P+046),5,ZERO=NOBLANK,ALIGN=LEFT               
         EDIT  (B4,BTKBINV3),(08,P+055),5,ZERO=NOBLANK,ALIGN=LEFT               
*                                                                               
         EDIT  (B1,BTKFRGV1),(03,P+065),0,ZERO=NOBLANK,ALIGN=LEFT               
         CLI   BTKFRGV1,0                                                       
         BE    PRTR25H                                                          
         LA    R5,BTKFRGV1                                                      
         BAS   RE,PRTR40                                                        
         MVC   P+070(12),0(R6)     GET SPACE NAME                               
*                                                                               
PRTR25H  EDIT  (B1,BTKFRGV2),(03,P+085),0,ZERO=NOBLANK,ALIGN=LEFT               
         CLI   BTKFRGV2,0                                                       
         BE    PRTR25M                                                          
         LA    R5,BTKFRGV2                                                      
         BAS   RE,PRTR40                                                        
         MVC   P+090(12),0(R6)     GET SPACE NAME                               
*                                                                               
PRTR25M  EDIT  (B1,BTKFRGV3),(03,P+105),0,ZERO=NOBLANK,ALIGN=LEFT               
         CLI   BTKFRGV3,0                                                       
         BE    PRTR25X                                                          
         LA    R5,BTKFRGV3                                                      
         BAS   RE,PRTR40                                                        
         MVC   P+110(12),0(R6)     GET SPACE NAME                               
*                                                                               
PRTR25X  DS    0H                                                               
         BAS   RE,RPRT             FINALLY PRINT EVERYTHING                     
         LA    R3,BINRECLQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     PRTR20                                                           
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
PRTR40   DS    0H                  SEARCH TABLE FOR SPACE NAME                  
         L      R6,VFRGTAB                                                      
PRTR40H  CLI    0(R6),X'FF'        END OF TABLE?                                
         BNE    *+6                                                             
         DC     H'0'               ENTRY MUST BE FOUND IN TABLE!                
         CLC    14(1,R6),0(R5)     R5 POINTS TO MATCHING CODE                   
         BE     PRTR40X                                                         
         LA     R6,20(R6)          NEXT ENTRY IN FRGTAB                         
         B      PRTR40H                                                         
PRTR40X  BR     RE                                                              
*                                                                               
*                                                                               
*                                                                               
PRTR45   DS    0H                                                               
         BAS   RE,RPRT             SHIP A LINE                                  
*                                                                               
         L     RE,BTABCNT                                                       
         L     RF,BINCOUNT                                                      
         AHI   RF,-1               EXCLUDE LAST RECORD IN BINSRCH TAB           
         CR    RE,RF                                                            
         BE    PRTR45H                                                          
         MVC   P+05(39),=C'ERROR: INTERNAL COUNTERS DO NOT ADD UP!'             
         BAS   RE,RPRT                                                          
*                                                                               
PRTR45H  MVC   P+05(42),=C'TOTAL SPACE DESCRIPTION IN BINSRCH TABLE: '          
         EDIT  (B4,BTABCNT),(13,P+47),ZERO=NOBLANK,ALIGN=LEFT,         +        
               COMMAS=YES                                                       
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT             SHIP A LINE                                  
*                                                                               
PRTR50   DS    0H                  PRINTING SPACE DESP USAGES                   
*                                                                               
         GOTO1 =V(XSORT),DMCB,(0,ASPADTAB),6000,21,17,0                         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20                                                      
*                                                                               
         SR    R3,R3               TOTAL SPACE DESP COUNTER                     
         LA    R4,P+5              R4 POINTS TO PRINTLINE                       
         L     R5,ASPADTAB         R5 POINTS TO SPACE TABE                      
         SR    R6,R6               PER PRINTLINE COUNTER                        
         XC    TOTCNT,TOTCNT       USED FOR TOTALING                            
         XC    FULL,FULL           USED FOR LOOP CONTROL                        
*                                                                               
PRTR60   DS    0H                                                               
         L     RE,FULL                                                          
         AHI   RE,1                                                             
         ST    RE,FULL                                                          
         CHI   RE,6000                                                          
         BH    PRTR70              DONE                                         
*                                                                               
         OC    0(17,R5),0(R5)      SKIPPING BLANK ENTRIES ON TOP OF TAB         
         BZ    PRTR65              NOTHING TO PRINT, NEXT ENTRY                 
         AHI   R3,1                                                             
*                                                                               
         CHI   R6,3                THREE PER PRINTLINE                          
         BL    PRTR63                                                           
         BAS   RE,RPRT                                                          
         LA    R4,P+5              REPOINT TO PRINTLINE                         
         SR    R6,R6               RESET PER PRINTLINE COUNTER                  
*                                                                               
PRTR63   MVC   0(17,R4),0(R5)                                                   
         ICM   RE,15,17(R5)        KEEP TRACK OF GRAND TOTAL                    
         A     RE,TOTCNT                                                        
         ST    RE,TOTCNT                                                        
         EDIT  (B4,17(R5)),(10,20(R4)),ZERO=NOBLANK,ALIGN=LEFT,        +        
               COMMAS=YES                                                       
         LA    R4,35(R4)                                                        
         AHI   R6,1                                                             
PRTR65   LA    R5,21(R5)           NEXT ENTRY                                   
         B     PRTR60              CHK NEXT ENTRY IN TABLE                      
*                                                                               
*                                                                               
*                                                                               
PRTR70   DS    0H                                                               
         BAS   RE,RPRT             PRINT OUT WHATEVER LEFT IN PLINE             
         BAS   RE,RPRT                                                          
         MVC   P+05(36),=C'TOTAL OF UNIQUE SPACE DESCRIPTIONS: '                
         EDIT  (R3),(13,P+41),ZERO=NOBLANK,ALIGN=LEFT,COMMAS=YES                
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
         MVC   P+05(26),=C'TOTAL SPACE DESCRIPTIONS: '                          
         EDIT  (B4,TOTCNT),(13,P+31),ZERO=NOBLANK,ALIGN=LEFT,COMMAS=YES         
         BAS   RE,RPRT                                                          
*                                                                               
PRTRXX   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITTAB  NTR1                                                                   
*                                                                               
         XC    WKAREA,WKAREA                                                    
*                                                                               
         L     RE,=A(SPADTAB)                                                   
         ST    RE,ASPADTAB         ADDRESS OF SPACE DESCRIPTION TAB             
         L     RF,=F'126000'                                                    
         XCEF                                                                   
*                                                                               
* CLEAR BINSRCH TABLE AND SETUP IT'S PARAMETERS                                 
*                                                                               
         L     RE,=A(BINTAB)                                                    
         ST    RE,BINATAB          ADDRESS OF BINSRCH TABLE                     
         L     RF,=F'108000'       LENGTH OF BINSRCH TABLE                      
         XCEF                                                                   
         XC    BINPARMS(4),BINPARMS                                             
         XC    BINCOUNT(4),BINCOUNT                                             
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
CNTLNQ   EQU   8+30                                                             
*                                                                               
INCNT    DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS READ'                                        
MEDBCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA B'                                 
MEDDCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA D'                                 
MEDICNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA I'                                 
MEDLCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA L'                                 
MEDMCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA M'                                 
MEDNCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA N'                                 
MEDSCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA S'                                 
MEDTCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA T'                                 
MEDVCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA V'                                 
MEDWCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA W'                                 
SPACNT   DS    PL8                                                              
         DC    CL30'TOTAL SPACE DESRIPTION ELEM'                                
USPACNT  DS    PL8                                                              
         DC    CL30'TOTAL UNIQUE SPACE IN TABLE'                                
         DC    X'FF'                               END OF COUNT TABLE           
*                                                                               
SKIPCNT  DS    PL8                                                              
         DC    CL30'MEDIA NOT B,D,I,L,M,N,S,T,V,W' TAKE OUT FOR NOW             
ERRSCNT  DS    PL8                                                              
         DC    CL30'TOTAL INCORRECT SPACE DESP'    TAKE OUT FOR NOW             
MEDOCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA O'    NOT WORKING RIGHT            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BINPARMS DC    A(0)                                                             
BINATAB  DC    A(0)                                                             
BINCOUNT DC    A(0)                RECORD COUNT                                 
         DC    A(BINRECLQ)         LENGTH                                       
         DC    AL1(0),AL3(BINKEYLQ)                                             
BINMAX   DC    A(3000)             MAX NUMBER OF RECORDS                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TOTCNT   DS    F                                                                
BTABCNT  DS    F                                                                
SAVEKEY  DS    CL32                                                             
WKAREA   DS    CL255                                                            
VFRGTAB  DS    V                                                                
ASPADTAB DS    A                                                                
ERRSPSW  DS    XL1                 ERROR SPACE DESP SWITCH                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BINTAB   CSECT                     TABLE FOR BINSRCH                            
*                                                                               
         DS    3000CL36                                                         
*                                                                               
BTABXQ   EQU   *                                                                
*                                                                               
BTABRECD DSECT                     DSECT OF CL36 IN BINTAB                      
*                                                                               
BTKSPA   DS    CL17                SPACE DESCRIPTION (KEY)                      
BTKBINV1 DS    XL4                 BINARY VALUE 1 (FROM PPSPCVAL)               
BTKBINV2 DS    XL4                 BINARY VALUE 2 (FROM PPSPCVAL)               
BTKBINV3 DS    XL4                 BINARY VALUE 3 (FROM PPSPCVAL)               
BTKFRGV1 DS    X                   FRAGMENT VALUE 1 (FROM PPSPCVAL)             
BTKFRGV2 DS    X                   FRAGMENT VALUE 1 (FROM PPSPCVAL)             
BTKFRGV3 DS    X                   FRAGMENT VALUE 1 (FROM PPSPCVAL)             
BTKUSCNT DS    XL4                 USAGE COUNTER                                
*                                                                               
BINKEYLQ EQU   L'BTKSPA                                                         
BINRECLQ EQU   *-BTKSPA                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SPADTAB  CSECT                     TABLE FOR SPACE DESCRIPTION ERRORS           
*                                                                               
         DS    6000CL21            17 FOR SPACE DESP AND 4 FOR COUNT            
*                                                                               
ERRSTXQ  EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PP02WRKD DSECT                                                                  
ELCODE   DS    X                                                                
ELEM     DS    XL255                                                            
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066PPREPSU02 07/18/18'                                      
         END                                                                    
