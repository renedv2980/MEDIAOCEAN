*          DATA SET APGHFILMC  AT LEVEL 002 AS OF 01/29/97                      
*                                                                               
*PHASE ACHFILMC,+0                                                              
         TITLE 'APG HOOK - LOWE ALLOCATION SALAIES FUDGE'                       
ACHFILMC CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R7                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         SPACE 2                                                                
         CLI   ONEXONLY,YES                                                     
         BE    HOOK00                                                           
         MVI   ONEXONLY,YES                                                     
         MVI   MYCOUNT,0                                                        
         XC    NPEOPLE,NPEOPLE                                                  
         LA    R3,SALTAB                                                        
         ST    R3,ANEXTSAL                                                      
*                                                                               
HOOK00   ICM   R6,15,HOOKAREC      A(LASTREC)                                   
         BZ    XIT                                                              
         CLI   HOOKNUM,1           BUILD TABLE OF PEOPLE WITH TOTAL SAL         
         BNE   HOOK50                                                           
         USING RSORTD,R6                                                        
*                                                                               
         CLI   RSRTREP,3                                                        
         BE    HOOK10                                                           
         CLI   RSRTREP,4                                                        
         BE    HOOK10                                                           
         CLI   RSRTREP,12                                                       
         BE    HOOK10                                                           
         CLI   RSRTREP,13                                                       
         BE    HOOK10                                                           
*                                                                               
         CLI   RSRTREP,5                                                        
         BE    TABSAL                                                           
HOOK10   ZAP   RSRTCL5,RSRTCL1                                                  
         AP    RSRTCL5,RSRTCL3                                                  
         ZAP   RSRTCL6,RSRTCL5                                                  
         B     XIT                                                              
HOOK50   CLI   HOOKNUM,2           PUT TOTAL SAL IN COLUMN 1                    
         BNE   XIT                                                              
*                                                                               
         CLI   RSRTREP,17                                                       
         BH    FILSAL80                                                         
         CLI   RSRTREP,16                                                       
         BE    HOOK60                                                           
         CLI   RSRTREP,17                                                       
         BE    HOOK60                                                           
         CLI   RSRTREP,8                                                        
         BE    HOOK60                                                           
         CLI   RSRTREP,9                                                        
         BNE   HOOK80                                                           
HOOK60   ZAP   RSRTCL8,RSRTCL3                                                  
         AP    RSRTCL8,RSRTCL6                                                  
         ZAP   RSRTCL9,RSRTCL8                                                  
HOOK80   B     FILSAL                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BULID TABLE OF PEOPLE WITH THEIR TOTAL SALARIES              *         
*---------------------------------------------------------------------*         
TABSAL   ICM   R6,15,HOOKAREC      A(LASTREC)                                   
         BZ    XIT                                                              
         USING RSORTD,R6                                                        
         USING SALTABD,R3                                                       
TABSAL10 L     R3,ANEXTSAL         ADDRESS OF NEXT PERSON SALARY                
         MVC   SALACCT,RSRTR1+2    MOVE IN PERSONS ACCOUNT NUMBER               
         ZAP   SALARY,RSRTCL1      MOVE IN THERE SALARY BUD1 F                  
         ZAP   SALF62,RSRTCL2      MOVE IN THERE SALARY BUD2 F62                
         ZAP   SALF61,RSRTCL3      MOVE IN THERE SALARY BUD2 F61                
         ZAP   SALFSC,SALF62       MOVE IN THERE SALARY BUD2 F62                
         AP    SALFSC,SALF61       ADD BUD2 F61 FOR FULL YEAR                   
         LA    R3,SALLNQ(R3)       BUMP TO NEXT SPACE FOR SALARY                
         ST    R3,ANEXTSAL                                                      
*                                                                               
         L     R1,NPEOPLE                                                       
         AH    R1,=H'01'                                                        
         ST    R1,NPEOPLE                                                       
         CH    R1,=Y(SALTABQ)                                                   
         BNH   XIT_NO              DON'T SAY ANY RECORDS                        
         DC    H'0'                TABLE FULL                                   
         EJECT                                                                  
FILSAL   L     R3,ANEXTSAL                                                      
         MVI   0(R3),EOT           MARK END OF TABLE                            
         LA    R3,SALTAB           LOAD ADDRESS OF BEGINING OF SALTAB           
         ICM   R6,15,HOOKAREC      A(LASTREC)                                   
         BZ    XIT                                                              
*                                                                               
         CLI   QOPT7,C'D'                                                       
         BNE   FILSAL05                                                         
         L     R1,MYCOUNT                                                       
         CH    R1,=H'10'                                                        
         BH    FILSAL05                                                         
         AH    R1,=H'01'                                                        
         ST    R1,MYCOUNT                                                       
         LA    R0,RSRTLNQ                                                       
         GOTO1 PRNTBL,DMCB,(8,=C'LAST REC'),(R6),C'DUMP',(R0),=C'3D',  X        
               (C'P',PRINT)                                                     
*---------------------------------------------------------------------*         
*        BUILD PERSONS ACCOUNT TO FIND IN TABLE                       *         
*---------------------------------------------------------------------*         
FILSAL05 DS    0H                                                               
         MVC   R1RACCT,SPACES                                                   
         MVC   R1ROFF,RSRTR1+2                                                  
         CLI   RSRTREP,14                                                       
         BNL   FILSAL07                                                         
         MVC   R1RDEPT,RSRTR4+2                                                 
         MVC   R1RSDEPT,RSRTR5+2                                                
         MVC   R1RPER,RSRTR6+2                                                  
         B     FILSAL10                                                         
FILSAL07 MVC   R1RDEPT(11),RSRTR6+2                                             
*                                                                               
FILSAL10 CLI   SALACCT,EOT                                                      
         BNE   *+6                 SHOULD NEVER BUMP BAST END OF TABLE          
         DC    H'0'                                                             
         CLC   R1RACCT,SALACCT                                                  
         BE    FILSAL20                                                         
         LA    R3,SALLNQ(R3)       BUMP TO NEXT PERSON                          
         B     FILSAL10                                                         
*                                                                               
FILSAL20 DS    0H                                                               
         CLI   RSRTREP,14                                                       
         BE    FILSAL25                                                         
         CLI   RSRTREP,15                                                       
         BE    FILSAL25                                                         
         CLI   RSRTREP,6                                                        
         BE    FILSAL25                                                         
         CLI   RSRTREP,7                                                        
         BNE   FILSAL30                                                         
FILSAL25 ZAP   RSRTCL1,SALARY      PUT IN NEW TOTAL SALARY                      
         B     FILSAL40                                                         
FILSAL30 ZAP   RSRTCL1,SALF62      PUT IN NEW TOTAL SALARY                      
         ZAP   RSRTCL4,SALF61      PUT IN NEW TOTAL SALARY                      
         ZAP   RSRTCL7,SALFSC      PUT IN NEW TOTAL SALARY                      
*                                                                               
FILSAL40 DS    0H                                                               
         CLI   RSRTREP,14                                                       
         BE    FILSAL45                                                         
         CLI   RSRTREP,15                                                       
         BE    FILSAL45                                                         
         CLI   RSRTREP,16                                                       
         BE    FILSAL42                                                         
         CLI   RSRTREP,17                                                       
         BE    FILSAL42                                                         
         B     FILSAL50                                                         
*                                                                               
FILSAL42 DS    0H                                                               
         CP    RSRTCL4,=P'0'                                                    
         BE    FILSAL43                                                         
         ZAP   MYDIV1,RSRTCL6                                                   
         ZAP   MYDIV2,RSRTCL4                                                   
         MP    MYDIV1,=P'10000'                                                 
         DP    MYDIV1,MYDIV2                                                    
         ZAP   RSRTCL5,MYDIV1(8)                                                
FILSAL43 CP    RSRTCL7,=P'0'                                                    
         BE    FILSAL45                                                         
         ZAP   MYDIV1,RSRTCL9                                                   
         ZAP   MYDIV2,RSRTCL7                                                   
         MP    MYDIV1,=P'10000'                                                 
         DP    MYDIV1,MYDIV2                                                    
         ZAP   RSRTCL8,MYDIV1(8)                                                
FILSAL45 DS    0H                                                               
         CP    RSRTCL1,=P'0'                                                    
         BE    FILSAL50                                                         
         ZAP   MYDIV1,RSRTCL3                                                   
         ZAP   MYDIV2,RSRTCL1                                                   
         MP    MYDIV1,=P'10000'                                                 
         DP    MYDIV1,MYDIV2                                                    
         ZAP   RSRTCL2,MYDIV1(8)                                                
*                                                                               
FILSAL50 DS    0H                  ONLY REPORTS 14,15,16,17 WILL GET            
         CLI   RSRTREP,14          TO THIS POINT                                
         BL    XIT                                                              
         ICM   R6,15,HOOKAREC                                                   
         LA    R3,AMTENT                                                        
         LA    R4,AMTR1                                                         
         LA    R5,RSRTCL1                                                       
FILSAL55 DS    0H                                                               
         CLI   RSRTREP,15          TO THIS POINT                                
         BH    FILSAL60                                                         
         CH    R3,=H'6'                                                         
         BH    FILSAL60                                                         
         OI    L'RSRTCL1-1(R5),X'0C'                                            
FILSAL60 AP    0(AMTRCP,R4),0(L'RSRTCL1,R5)                                     
         LA    R4,AMTRCP(R4)                                                    
         LA    R5,L'RSRTCL1(R5)                                                 
         BCT   R3,FILSAL55                                                      
         B     XIT_NO                                                           
*                                                                               
FILSAL80 DS    0H                  ONLY REPORTS 18,19,20,21 (RECAPS)            
         ICM   R6,15,HOOKAREC      WILL GET TO THIS POINT                       
**       MVC   RSRTR4(1),RSRTR5                                                 
**       MVC   RSRTR3(1),RSRTR5                                                 
**       MVC   RSRTR2(1),RSRTR5                                                 
**       MVC   RSRTR1(1),RSRTR5                                                 
*                                                                               
         LA    R3,AMTENT           PUT ACCUMULATED AMOUNTS INTO                 
         LA    R4,AMTR1            RECAP ACCUMULATORS                           
         LA    R5,RSRTCL1                                                       
FILSAL85 ZAP   0(L'RSRTCL1,R5),0(AMTRCP,R4)                                     
         LA    R4,AMTRCP(R4)                                                    
         LA    R5,L'RSRTCL1(R5)                                                 
         BCT   R3,FILSAL85                                                      
*                                                                               
         LA    R3,AMTENT           IF THERE WERE NO DOLLARS EXCLUDE             
         LA    R4,AMTR1            THIS RECAP RECORD FROM PRINTING              
FILSAL86 CP    0(AMTRCP,R4),=P'0'                                               
         BNE   FILSAL87                                                         
         LA    R4,AMTRCP(R4)                                                    
         BCT   R3,FILSAL86                                                      
         B     XIT_NO                                                           
*                                                                               
FILSAL87 LA    R3,AMTENT           CLEAR OUT TABLE FOR NEXT PASS                
         LA    R4,AMTR1                                                         
FILSAL88 ZAP   0(AMTRCP,R4),=P'0'                                               
         LA    R4,AMTRCP(R4)                                                    
         BCT   R3,FILSAL88                                                      
*                                                                               
XIT      SR    RE,RE               THIS XIT SETS CC TO KEEP RECORD              
XIT_NO   LTR   RE,RE               THIS XIT SETS CC TO ELEMINATE RECORD         
         XIT1                                                                   
         EJECT                                                                  
SALTABQ  EQU   500                                                              
*                                                                               
ANEXTSAL DS    A(0)                                                             
*                                                                               
R1RACCT  DS    CL12                PERSONS ACCOUNT NUMBER                       
         ORG   R1RACCT                                                          
R1ROFF   DS    CL1                 OFFICE                                       
R1RDEPT  DS    CL2                 DEPARTMENT                                   
R1RSDEPT DS    CL2                 SUB-DEPARTMENT                               
R1RPER   DS    CL7                 PERSONAL ID                                  
*                                                                               
MYCOUNT  DC    A(0)                                                             
NPEOPLE  DS    F                                                                
ONEXONLY DC    AL1(NO)                                                          
DIVDUB   DS    PL16                                                             
         ORG   DIVDUB                                                           
QNTDUB   DS    PL8                                                              
RNDDUB   DS    PL8                                                              
VERTTOT  DS    PL8                                                              
*                                                                               
MYDIV1   DS    PL16                                                             
MYDIV2   DS    PL8                                                              
*                                                                               
AMTR1    DC    PL8'0'                                                           
AMTRCP   EQU   *-AMTR1                                                          
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
AMTENT   EQU   (*-AMTR1)/AMTRCP                                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SALTAB   DS    0F                                                               
         DS    (SALTABQ)CL(SALLNQ)                                              
         DC    AL1(EOT)                                                         
*---------------------------------------------------------------------*         
*        SALARY TABLE                                                 *         
*---------------------------------------------------------------------*         
SALTABD  DSECT                                                                  
SALACCT  DS    CL12                                                             
SALARY   DS    PL8                                                              
SALF62   DS    PL8                                                              
SALF61   DS    PL8                                                              
SALFSC   DS    PL8                                                              
SALLNQ   EQU   *-SALTABD                                                        
*---------------------------------------------------------------------*         
*        SORT RECORD AS IT COMES OUT OF SORTER (NOT REBUILT)          *         
*---------------------------------------------------------------------*         
RSORTD   DSECT                                                                  
RSRTR1   DS    CL16                ROW 1                                        
RSRTR2   DS    CL16                ROW 2                                        
RSRTR3   DS    CL16                ROW 3                                        
RSRTR4   DS    CL16                ROW 4                                        
RSRTR5   DS    CL16                ROW 5                                        
RSRTR6   DS    CL16                ROW 6                                        
RSRTREP  DS    CL1                 REPORT NUMBER                                
RSRTCPY  DS    CL1                 REPORT NUMBER OF COPIES                      
RSRTTYP  DS    CL2                 REPORT TYPE                                  
RSRTNM1  DS    CL36                NAME 1                                       
RSRTNM2  DS    CL36                NAME 2                                       
RSRTNM3  DS    CL36                NAME 3                                       
RSRTNM4  DS    CL36                NAME 4                                       
RSRTNM5  DS    CL36                NAME 5                                       
RSRTNM6  DS    CL36                NAME 6                                       
RSRTCL1  DS    PL8                 COLUMN 1                                     
RSRTCL2  DS    PL8                 COLUMN 2                                     
RSRTCL3  DS    PL8                 COLUMN 3                                     
RSRTCL4  DS    PL8                 COLUMN 4                                     
RSRTCL5  DS    PL8                 COLUMN 5                                     
RSRTCL6  DS    PL8                 COLUMN 6                                     
RSRTCL7  DS    PL8                 COLUMN 7                                     
RSRTCL8  DS    PL8                 COLUMN 8                                     
RSRTCL9  DS    PL8                 COLUMN 9                                     
RSRTLNQ  EQU   *-RSORTD                                                         
         EJECT                                                                  
*        INCLUDED HERE                                                          
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACAPGDSECT                                                             
         PRINT OFF                                                              
       ++INCLUDE ACAPGDSECT                                                     
         PRINT ON                                                               
*        ACAPGMAND                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGMAND                                                      
         PRINT ON                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002APGHFILMC 01/29/97'                                      
         END                                                                    
