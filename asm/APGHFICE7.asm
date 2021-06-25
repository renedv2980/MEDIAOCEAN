*          DATA SET APGHFICE7  AT LEVEL 001 AS OF 08/27/93                      
*                                                                               
*PHASE ACHFICE7,+0                                                              
         TITLE 'APG HOOK FOR CONSOLIDATED REVENUE BY MONTH'                     
ACHFICE7 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT REC                             
         USING R1SORTD,R5                                                       
*                                                                               
         SR    R6,R6                                                            
         CLI   HOOKNUM,1                                                        
         BE    HOOK01                                                           
         IC    R6,S1REPORT         GET REPORT NUMBER                            
         BCTR  R6,0                DECREMENT BY ONE                             
         MH    R6,=Y(ADJREPQ)      LENGTH OF TABLE ENTRY                        
         LA    R6,ADJREP(R6)                                                    
         SR    R7,R7                                                            
         ICM   R7,3,1(R6)          LOAD DISPLACEMENT TO SL CHAR CODE            
         AR    R7,R5               POINT TO SORT RECORD NAME                    
         B     HKPUTSRT                                                         
*                                                                               
HOOK01   SR    R3,R3                                                            
         IC    R3,PERIOD           INDEX TO START DATE OR ACCUM LINE            
         IC    R4,PERIOD+1         INDEX TO END DATE                            
         SR    R4,R3               NUMBER OF MONTHS REQUESTED                   
         AH    R4,=H'01'           ADJUST BY ONE                                
         STC   R4,NMTHS                                                         
         SH    R3,=H'13'           BUMP TO PRIOR YEAR ACCUMS                    
         MH    R3,=H'08'           POINT TO CORRECT MONTHS IN ACCUMS            
         LA    RE,DEBITS           LINE 2 OF DEBITS                             
         MH    RE,WLINE                                                         
         A     RE,AACCUM                                                        
*        CLC   CURRCON+1(2),=C'12'                                              
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         AR    RE,R3               POINTING TO 12 MONTHS OF PRIOR CR            
*                                                                               
         LA    R1,12                                                            
         LA    RF,PRIOR            MY PRIOR ACCUM LINE                          
         LA    R3,MONDIFF                                                       
HK10     EQU   *                                                                
         ZAP   0(8,RF),0(8,RE)     ZAP INTO MY 12 MONTHS PRIOR LINE             
         ZAP   0(8,R3),0(8,RE)     ZAP INTO MY 12 MONTHS MONTH DIFF             
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,HK10                                                          
*                                                                               
         CLI   ONETIME,YES                                                      
         BE    XIT                                                              
         MVI   ONETIME,YES                                                      
         LA    RF,RECODE                                                        
         ST    RF,SAVEREG                                                       
         B     XIT                                                              
         EJECT                                                                  
HKPUTSRT CLI   HOOKNUM,2           HOOK CALL BEFORE PUTING TO SORT              
         BNE   HKMERG                                                           
*                                                                               
         L     RF,SAVEREG          FORCE RECORDS IN RECODE ORDER                
HK12     CLI   0(RF),EOTT                                                       
         BNE   HK14                                                             
         LA    RF,RECODE                                                        
         ST    RF,SAVEREG                                                       
*                                                                               
HK14     MVC   0(1,R7),0(RF)       RECODED SUPERLEDGER RECORD CODE              
         SR    RE,RE                                                            
         ICM   RE,3,3(R6)          LOAD DISPLACEMENT TO SL NAME ENTRY           
         AR    RE,R5               POINT TO SORT RECORD NAME                    
         MVC   0(36,RE),1(RF)      SUPERLEDGER NAME                             
         LA    RF,37(RF)           BUMP TO NEXT CODE FOR NEXT TIME IN           
         ST    RF,SAVEREG                                                       
*                                                                               
         LA    R1,12               CLEAR ALL PERCENT ACCUMS                     
         LA    RF,S1MON1                                                        
         ZAP   8(8,RF),=PL8'0'                                                  
         LA    RF,16(RF)           BUMP NEXT ACCUM                              
         BCT   R1,*-10                                                          
*                                                                               
         CLI   0(R7),CURLN         CURRENT LINE OF FROM SUPERLEDGER             
         BNE   HK20                                                             
         IC    R1,NMTHS                                                         
         LA    RE,CURRENT                                                       
         LA    RF,S1MON1                                                        
HK15     ZAP   0(8,RE),0(8,RF)     SAVE CURRENT AMTS                            
         LA    RE,8(RE)            BUMP TO NEXT ACCUM                           
         LA    RF,16(RF)           BUMP TO NEXT ACCUM                           
         BCT   R1,HK15                                                          
*                                                                               
HK20     CLI   0(R7),PRILN         PRIOR LINE OF FROM SUPERLEDGER               
         BNE   HK30                                                             
         LA    R1,12                                                            
         LA    RF,PRIOR                                                         
         LA    RE,S1MON1           FIRST MONTH BUCKET                           
HK25     ZAP   0(8,RE),0(8,RF)                                                  
         LA    RE,16(RE)           BUMP TO NEXT ACCUM                           
         LA    RF,8(RF)            BUMP TO NEXT ACCUM                           
         BCT   R1,HK25                                                          
*                                                                               
HK30     CLI   0(R7),MONLN         MONTHY DIFFERENCE FROM SUPERLEDGER           
         BNE   HK40                                                             
         IC    R1,NMTHS                                                         
         LA    RF,MONDIFF          CURRENT DIFFENCE                             
         LA    RE,S1MON1           FIRST MONTH BUCKET                           
         LA    R3,YTDDIFF          YTD DIFFERENCE                               
HK35     SP    0(8,RE),0(8,RF)                                                  
         ZAP   0(8,R3),0(8,RE)     YTD DIFFERENCE                               
         LA    RE,16(RE)           BUMP TO NEXT ACCUM                           
         LA    RF,8(RF)            BUMP TO NEXT ACCUM                           
         LA    R3,8(R3)            BUMP TO NEXT ACCUM                           
         BCT   R1,HK35                                                          
*                                                                               
HK40     CLI   0(R7),YTDLN         YTD DIFFERENCE FROM SUPERLEDGER              
         BNE   HK100                                                            
         IC    R1,NMTHS                                                         
         LA    RF,YTDDIFF          MONTHLY DIFFENCE                             
         LA    RE,S1MON1           FIRST MONTH BUCKET                           
HK45     ZAP   0(8,RE),0(8,RF)                                                  
         LA    RE,16(RE)           BUMP TO NEXT ACCUM                           
         LA    RF,8(RF)            BUMP TO NEXT ACCUM                           
         BCT   R1,HK45                                                          
*                                                                               
         IC    R1,NMTHS                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNP   HK100                                                            
         LA    RE,S1MON1           FIRST MONTH BUCKET                           
HK50     AP    16(8,RE),0(8,RE)                                                 
         LA    RE,16(RE)           BUMP TO NEXT ACCUM                           
         BCT   R1,HK50                                                          
***                                                                             
HK100    DS    0H                                                               
         CLI   ACTREP,2                     CLEARING RESPECTIVE SORT            
         BNE   HK110                        FIELD TO FORCE RECAPPING            
         MVC   S1OFFICE(14),SPACES          AT ONE HIGHER LEVEL OF              
         MVC   S1OFFICE(1),=C'1'            AT ONE HIGHER LEVEL OF              
         MVC   SONAME,=CL36'TOTALS'                                             
HK110    CLI   ACTREP,4                     TOTALS                              
         BNE   HK120                                                            
         MVC   S1DIV(14),SPACES                                                 
         MVC   S1DIV(1),=C'1'                                                   
         MVC   S1NAME,=CL36'TOTALS'                                             
HK120    CLI   ACTREP,6                                                         
         BNE   HK130                                                            
         MVC   S1CLI(14),SPACES                                                 
         MVC   S1CLI(1),=C'1'                                                   
         MVC   S2NAME,=CL36'TOTALS'                                             
HK130    CLI   ACTREP,8                                                         
         BNE   HK140                                                            
         MVC   S1PRD(14),SPACES                                                 
         MVC   S1PRD(1),=C'1'                                                   
         MVC   S3NAME,=CL36'TOTALS'                                             
HK140    DS    0H                                                               
***                                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
HKMERG   CLI   HOOKNUM,3           HOOK CALL AFTER MERGER                       
         BE    *+6                 CALCULATE PERCENT                            
         DC    H'0'                                                             
         SR    R1,R1                                                            
*                                                                               
         CLI   0(R7),PRILN         PRIOR YEAR RECORD?                           
         BNE   HK60                                                             
         ZAP   S1YTDCUR,=PL8'0'    CLEAR OUT YTD CURRENT                        
         IC    R1,NMTHS                                                         
         LA    RF,PRIOR                                                         
         LA    RE,S1MON1                                                        
HK55     ZAP   0(8,RF),0(8,RE)                                                  
         LA    RE,16(RE)                                                        
         LA    RF,8(RF)                                                         
         BCT   R1,HK55                                                          
         B     HK64                                                             
*                                                                               
HK60     IC    R1,NMTHS                                                         
         LA    R2,12                                                            
         SR    R2,R1               NUMBER OF MONTHS REMAINING                   
         BNP   HK64                                                             
         MH    R1,=H'16'           POINT TO CORRECT ACCUM                       
         LA    RF,S1MON1(R1)                                                    
         ZAP   0(8,RF),=PL8'0'                                                  
         LA    RF,16(RF)           BUMP TO NEXT ACCUM                           
         BCT   R2,*-10                                                          
*                                                                               
HK64     CLI   0(R7),YTDLN         YTD DIFFERENCE RECORD?                       
         BNE   HK70                                                             
         ZAP   S1YTDPRI,=PL8'0'    CLEAR OUT YTD PRIOR                          
         ZAP   S1YTDCUR,=PL8'0'    CLEAR OUT YTD CURRENT                        
         IC    R1,NMTHS                                                         
         LA    RF,YTDDIFF                                                       
         LA    RE,S1MON1                                                        
HK65     ZAP   0(8,RF),0(8,RE)                                                  
         LA    RE,16(RE)                                                        
         LA    RF,8(RF)                                                         
         BCT   R1,HK65                                                          
*                                                                               
HK70     CLI   0(R7),PERLN         PERCENT RECORD?                              
         BNE   HK80                                                             
         ZAP   S1YTDPRI,=PL8'0'    CLEAR OUT YTD PRIOR                          
         ZAP   S1YTDCUR,=PL8'0'    CLEAR OUT YTD CURRENT                        
         IC    R1,NMTHS                                                         
         LA    RF,YTDDIFF                                                       
         LA    RE,PRIOR                                                         
         LA    R3,S1MON1                                                        
         ZAP   PRIORYTD,=PL8'0'                                                 
HK75     ZAP   8(8,R3),=PL8'0'                                                  
         AP    PRIORYTD,0(8,RE)    ADD UP PRIOR MONTHS                          
         CP    PRIORYTD,=PL8'0'    DON'T DIVIDE BY ZERO                         
         BE    HK78                                                             
         ZAP   MYDUB,0(8,RF)                                                    
         MP    MYDUB,=P'1000000'                                                
         DP    MYDUB,PRIORYTD      ( YTD DIFF / PRIOR PERIOD TO DATE)           
         ZAP   8(8,R3),MYDUB(8)                                                 
         CP    8(8,R3),=P'1000000'   MAX NUMBER 100%                            
         BL    HK76                                                             
         ZAP   8(8,R3),=P'1000000'                                              
HK76     CP    8(8,R3),=P'-1000000'  MIN NUMBER -100%                           
         BH    HK78                                                             
         ZAP   8(8,R3),=P'-1000000'                                             
HK78     ZAP   0(8,R3),=PL8'0'     CLEAR OUT ADJACENT ACCUM                     
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         LA    R3,16(R3)                                                        
         BCT   R1,HK75                                                          
*                                                                               
HK80     CLI   0(R7),MONLN                                                      
         BE    HK85                                                             
         CLI   0(R7),CURLN                                                      
         BE    HK86                                                             
         B     HK90                                                             
HK85     ZAP   S1YTDCUR,=PL8'0'    CLEAR OUT YTD CURRENT                        
HK86     ZAP   S1YTDPRI,=PL8'0'    CLEAR OUT YTD PRIOR                          
HK90     B     XIT                                                              
         SPACE 4                                                                
XIT      SR    R0,R0               THIS XIT SETS CC TO YES                      
XITA     XIT1                                                                   
         EJECT                                                                  
*                                                                               
RECODE   DS    0F                                                               
         DC    AL1(PRILN),CL36'PRIOR YEAR'                                      
         DC    AL1(CURLN),CL36'CURRENT YEAR'                                    
         DC    AL1(MONLN),CL36'MONTHLY DIFFERENCE'                              
         DC    AL1(YTDLN),CL36'YEAR-TO-DATE DIFFERENCE'                         
         DC    AL1(PERLN),CL36'YEAR-TO-DATE % CHANGE'                           
         DC    AL1(0)                                                           
*                                                                               
ADJREP   DS    0F                                                               
         DC    AL1(1),AL2(S1CHAR-S1REPORT,S1NAME-S1REPORT)                      
ADJREPQ  EQU   *-ADJREP                                                         
         DC    AL1(2),AL2(S1CHAR-S1REPORT,S1NAME-S1REPORT)                      
         DC    AL1(3),AL2(S2CHAR-S1REPORT,S2NAME-S1REPORT)                      
         DC    AL1(4),AL2(S2CHAR-S1REPORT,S2NAME-S1REPORT)                      
         DC    AL1(5),AL2(S3CHAR-S1REPORT,S3NAME-S1REPORT)                      
         DC    AL1(6),AL2(S3CHAR-S1REPORT,S3NAME-S1REPORT)                      
         DC    AL1(7),AL2(S4CHAR-S1REPORT,S4NAME-S1REPORT)                      
         DC    AL1(8),AL2(S4CHAR-S1REPORT,S4NAME-S1REPORT)                      
         DC    AL1(0)                                                           
*                                                                               
ONETIME  DC    AL1(NO)                                                          
EOTT     EQU   0                   END OF TABLE                                 
DEBITS   EQU   1                                                                
CREDITS  EQU   2                                                                
PRILN    EQU   C'B'                                                             
CURLN    EQU   C'D'                                                             
MONLN    EQU   C'F'                                                             
YTDLN    EQU   C'H'                                                             
PERLN    EQU   C'J'                                                             
*                                                                               
PRIOR    DS    13PL8                                                            
CURRENT  DS    13PL8                                                            
MONDIFF  DS    13PL8                                                            
YTDDIFF  DS    13PL8                                                            
YTDCHA   DS    13PL8                                                            
*                                                                               
MYDUB    DS    PL16                                                             
PRIORYTD DS    PL8                                                              
*                                                                               
SAVEREG  DS    A                                                                
NMTHS    DS    XL1                                                              
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER REPTAB                                            
*                                                                               
R1SORTD  DSECT COVERS SORT RECORD PASSED FROM ACAPGUTILS                        
S1REPORT DS    XL1                 REPORT NUMBER                                
         DS    XL1                 NUMBER OF COPIES                             
S1OFFICE DS    CL1                 ROW 1 OFFICE RA(1)                           
         DS    CL13                                                             
*                                                                               
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1DIV    DS    CL1                 ROW 2 DIVISION CODE RA+1(1)                  
         DS    CL13                                                             
*                                                                               
         ORG   S1DIV                                                            
S1CHAR   DS    CL1                 IF REPORT = 1     AC(1)                      
         DS    CL13                                                             
*                                                                               
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1CLI    DS    CL3                 ROW 3 CLIENT CODE RA+2(3)                    
         DS    CL11                                                             
*                                                                               
         ORG   S1CLI                                                            
S2CHAR   DS    CL1                 IF REPORT = 2     AC(1)                      
         DS    CL13                                                             
*                                                                               
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1PRD    DS    CL7                 ROW 4 PRODUCT CODE RA+5(7)                   
         DS    CL7                                                              
*                                                                               
         ORG   S1PRD                                                            
S3CHAR   DS    CL1                 IF REPORT = 3     AC(1)                      
         DS    CL13                                                             
*                                                                               
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S4CHAR   DS    CL1                 IF REPORT = 4     AC(1)                      
         DS    CL13                                                             
*                                                                               
REPORT   EQU   *-R1SORTD           DISP TO REPORT NUMBER                        
ACTREP   DS    XL1                 REPORT NUMBER                                
         DS    XL1                 NUMBER COPIES                                
         DS    XL2                 2 BLANKS                                     
SONAME   DS    CL36                OFFICE NAME                                  
S1NAME   DS    CL36                DIVISION NAME OR SUPERLEDGER NAME            
S2NAME   DS    CL36                CLIENT NAME OR SUPERLEDGER NAME              
S3NAME   DS    CL36                PRODUCT NAME OR SUPERLEDGER NAME             
S4NAME   DS    CL36                SUPERLEDGER NAME                             
BUKLOC   EQU   *-R1SORTD           BEGINNING OF BUCKETS (COLUMNS)               
S1MON1   DS    PL8                 1ST MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON2   DS    PL8                 2ND MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON3   DS    PL8                 3RD MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON4   DS    PL8                 4TH MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON5   DS    PL8                 5TH MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON6   DS    PL8                 6TH MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON7   DS    PL8                 7TH MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON8   DS    PL8                 8TH MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON9   DS    PL8                 9TH MONTH CURRENT                            
         DS    PL8                 FOR PERCENT                                  
S1MON10  DS    PL8                 10THT MONTH CURRENT                          
         DS    PL8                 FOR PERCENT                                  
S1MON11  DS    PL8                 11TH MONTH CURRENT                           
         DS    PL8                 FOR PERCENT                                  
S1MON12  DS    PL8                 12TH MONTH CURRENT                           
         DS    PL8                 FOR PERCENT                                  
S1YTDCUR DS    PL8                 YTD CURRENT                                  
S1YTDPRI DS    PL8                 PRIOR FISCAL                                 
S1LEN    EQU   *-R1SORTD           LENGTH OF SORT RECORD                        
         EJECT                                                                  
*        INCLUDED HERE                                                          
*        ACREPWORKD                                                             
*        ACAPGWORKD                                                             
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACAPGWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001APGHFICE7 08/27/93'                                      
         END                                                                    
