*          DATA SET APGHM2HK   AT LEVEL 002 AS OF 09/02/93                      
*                                                                               
*PHASE ACHM2HK,+0                                                               
         TITLE 'APG HOOK FOR STDHRS YTD ADJUSTMENT'                             
ACHM2HK  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT REC                             
*                                                                               
HOOK01   CLI   HOOKNUM,1           HOOK CALL WHEN FORMING COLS                  
         BNE   HOOKMERG                                                         
         CLI   ONETIME,YES         DO ONLY ONE TIME IN                          
         BE    XIT                                                              
         MVI   ONETIME,YES                                                      
         LA    RF,STDHRS           MY STD HRS YTD ACCUM LINE                    
         SR    R3,R3                                                            
         IC    R3,PERIOD           INDEX TO START DATE OR ACCUM LINE            
         BCTR  R3,0                                                             
         MH    R3,=H'08'           POINT TO CORRECT MONTHS IN ACCUMS            
         LA    RE,30               LINE 30 IS APG STDHRS BY MONTH LINE          
         MH    RE,WLINE                                                         
         A     RE,AACCUM                                                        
         AR    RE,R3               NOW POINTING TO 12 MONTHS OF STDHRS          
*                                                                               
         LA    R1,12                                                            
HK10     EQU   *                                                                
         ZAP   0(8,RF),0(8,RE)     ZAP INTO MY 12 MONTHS STDHRS LINE            
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R1,HK10                                                          
*                                                                               
         LA    R1,12                                                            
         LA    RE,STDHRS                                                        
HK30     EQU   *                                                                
         ZAP   MYDUB,0(8,RE)       HK STD HRS ARE BASED ON 8HR DAYS             
         MP    MYDUB,=PL8'800'                                                  
         DP    MYDUB,=PL8'632'     NOT 6.32 AS APG HAS                          
         ZAP   0(8,RE),MYDUB(8)                                                 
         LA    RE,8(RE)                                                         
         BCT   R1,HK30                                                          
         B     XIT                                                              
         EJECT                                                                  
HOOKMERG CLI   HOOKNUM,2           HOOK CALL AFTER MERGER CALL                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING R1SORTD,R5                                                       
         LA    R1,12                                                            
         LA    RE,S1M12                                                         
         ZAP   S1STDPCT,=PL8'0'    INITIALIZE TO ZERO                           
HOOK10   EQU   *                                                                
         CP    0(8,RE),=PL8'0'     IF ZERO DON'T ADD IN MONTHLY STDHRS          
         BE    HOOK20                                                           
         LR    R2,R1                                                            
         BCTR  R2,0                                                             
         MH    R2,=H'08'                                                        
         LA    RF,STDHRS(R2)       POINT TO CORRECT MON STDHRS                  
         AP    S1STDPCT,0(8,RF)    ACCUM TO CREATE YTD STDHRS(ADJUSTED)         
HOOK20   EQU   *                                                                
         SH    RE,=H'08'                                                        
         BCT   R1,HOOK10                                                        
         B     XIT                                                              
         SPACE 4                                                                
XIT      SR    R0,R0               THIS XIT SETS CC TO YES                      
XITA     XIT1                                                                   
         EJECT                                                                  
*                                                                               
ONETIME  DC    AL1(NO)                                                          
STDHRS   DS    12PL8                                                            
MYDUB    DS    PL16                                                             
FLIPFLOP DS    PL2                                                              
         LTORG                                                                  
ENTLNQ   EQU   15                  7 BYTE EMPL CODE 5 YTD STD HRS               
MAXEMPL  EQU   1600                MAX NUMBER OF EMPLOYEES IN TABLE             
EMPTABLE DS    (MAXEMPL*ENTLNQ)C                                                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER REPTAB                                            
*                                                                               
R1SORTD  DSECT COVERS SORT RECORD PASSED FROM ACAPGUTILS                        
S1REPORT DS    XL1                 REPORT NUMBER                                
         DS    XL1                 NUMBER OF COPIES                             
S1OFFICE DS    CL1                 ROW 1 OFFICE RA(1)                           
         DS    CL13                                                             
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1EMP1   DS    CL6                 ROW 2 EMPLOYEE CODE RA+5(6)                  
         DS    CL8                                                              
         DS    CL64                SPARE                                        
REPORT   EQU   *-R1SORTD           DISP TO REPORT NUMBER                        
         DS    XL1                 REPORT NUMBER                                
         DS    XL1                 NUMBER COPIES                                
         DS    XL2                 2 BLANKS                                     
         DS    CL36                OFFICE CODE NAME                             
         DS    CL36                ROW 2 NAME                                   
         DS    CL36                SPARE                                        
         DS    CL36                SPARE                                        
         DS    CL36                SPARE                                        
         DS    CL36                SPARE                                        
BUKLOC   EQU   *-R1SORTD           BEGINNING OF BUCKETS (COLUMNS)               
S1BUD1   DS    PL8                 BUDGET - TARGET PERCENT                      
         DS    PL8                                                              
S1M1     DS    PL8                 PCT JAN                                      
S1M2     DS    PL8                 PCT FEB                                      
S1M3     DS    PL8                 PCT MAR                                      
S1M4     DS    PL8                 PCT APR                                      
S1M5     DS    PL8                 PCT MAY                                      
S1M6     DS    PL8                 PCT JUN                                      
S1M7     DS    PL8                 PCT JUL                                      
S1M8     DS    PL8                 PCT AUG                                      
S1M9     DS    PL8                 PCT SEP                                      
S1M10    DS    PL8                 PCT OCT                                      
S1M11    DS    PL8                 PCT NOV                                      
S1M12    DS    PL8                 PCT DEC                                      
S1HRS    DS    PL8                 TOTAL HOURS (1C ONLY)                        
S1STDPCT DS    PL8                 STAND HRS FOR YTD                            
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
**PAN#1  DC    CL21'002APGHM2HK  09/02/93'                                      
         END                                                                    
