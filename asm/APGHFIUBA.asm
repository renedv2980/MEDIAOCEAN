*          DATA SET APGHFIUBA  AT LEVEL 073 AS OF 02/23/04                      
*PHASE ACHFUBAA                                                                 
         TITLE 'CARAT CLIENT FILTER GROUPINGS'                                  
ACHFUBA  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         USING MAND,RA                                                          
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RC                                                       
         L     RC,HOOKAWRK                                                      
*                                                                               
         USING R1RECD,R7                                                        
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PUTHOOK -  MATCH ON FILTER VALUE THEN REPLACE WITH FILTER GROUP     *         
*---------------------------------------------------------------------*         
                                                                                
         USING FGROUP,R4                                                        
HOOK     CLI   HOOKTYPE,CMPTHK     PUTHOOK ?                                    
         BNE   HOOK200             NO                                           
         LA    R4,F5GROUP          FILTER 5 GROUPING                            
         CLI   HOOKNUM,2           PUTHOOK = 1 & 2  (FILTER 5)                  
         BNH   *+8                                                              
         LA    R4,F4GROUP          FILTER 4 GROUPING                            
*                                  PUTHOOK = 3 & 4  (FILTER 4)                  
         CLI   QOPT6,C'1'                                                       
         BNE   HK010                                                            
         L     R1,COUNT1                                                        
         CHI   R1,MAXDUMP                                                       
         BH    HK010                                                            
         AHI   R1,1                                                             
         ST    R1,COUNT1                                                        
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(8,=C'PHOOK BF'),(R7),C'DUMP',(R0),         X        
               =C'2D',(C'P',PRINT)                                              
                                                                                
HK010    L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AHI   R7,18                                                            
*                                                                               
HK170    CLI   0(R4),0             ANY MORE TO CHECK                            
         BE    HK190                                                            
         LA    RE,FGRDATA          POINT TO START OF FILTER VALUES              
         SR    RF,RF                                                            
         IC    RF,FGRLN            LENGTH OF ENTRY                              
         SHI   RF,FGRDATA-FGROUP   RF = NUMBER OF FILTER TO CHECK               
*                                                                               
HK175    CLC   0(1,RE),R1CDE1      MATCH ON FILTER                              
         BE    HK180               FOUND SO MOVE IN GROUP NUMBER                
         LA    RE,1(,RE)           NOT FOUND, TRY NEXT FILTER                   
         BCT   RF,HK175                                                         
*                                                                               
         IC    RF,FGRLN                                                         
         AR    R4,RF               TRY NEXT ENTRY                               
         B     HK170                                                            
*                                                                               
HK180    MVC   R1CDE1(1),FGROUP#   REPLACE FILTER WITH GROUP                    
         B     HK192                                                            
         DROP  R4                                                               
*                                                                               
HK190    MVI   R1CDE1,C'0'         UNKNOWN GROUP, DEFAULT                       
HK192    CLI   QOPT3,C' '          WAS A SPECIFIC GROUP REQUESTED               
         BE    HK199                                                            
         CLC   QOPT3,R1CDE1        DOES THIS GROUP MATCH REQUESTED              
         BNE   XITNO               GROUP - IF NOT DROP RECORD                   
         CLI   HOOKNUM,1           F5                                           
         BE    HK199                                                            
         CLI   HOOKNUM,3           F4                                           
         BE    HK199                                                            
*        CLI   R1REPNO,4                                                        
*        BE    HK195                                                            
*        CLI   R1REPNO,8                                                        
*        BE    HK195                                                            
*        CLI   R1REPNO,12                                                       
*        BE    HK195                                                            
*        CLI   R1REPNO,16                                                       
*        BNE   HK199                                                            
                                                                                
HK195    MVI   R1CDE1,C' '                                                      
                                                                                
HK199    CLI   QOPT7,C'1'                                                       
         BNE   XIT                                                              
         L     R1,COUNT1                                                        
         AHI   R1,1                                                             
         CHI   R1,MAXDUMP                                                       
         BH    XIT                                                              
         ST    R1,COUNT1                                                        
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(8,=C'PHOOK AF'),(R7),C'DUMP',(R0),         X        
               =C'2D',(C'P',PRINT)                                              
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*-------------------------------------------------------------------*           
*  SORTOUT - REPLACE ROW 1 WITH GROUP NAME                          *           
*-------------------------------------------------------------------*           
HOOK200  CLI   HOOKTYPE,CMSROT     SORTOUT ?                                    
         BNE   HK300                                                            
         CLI   HOOKNUM,1                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   QOPT6,C'2'                                                       
         BNE   HK205                                                            
         L     R1,COUNT2                                                        
         CHI   R1,MAXDUMP                                                       
         BH    HK205                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(10,=C'SHOOK BF=3'),(R7),C'DUMP',(R0),      X        
               =C'2D',(C'P',PRINT)                                              
                                                                                
HK205    LA    R4,GRPTAB                                                        
*                                                                               
HK210    CLI   0(R4),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(1,R4),R1CDE1      MATCH ON GROUP VALUE                         
         BE    HK200                                                            
         LA    R4,GRPLEN(,R4)                                                   
         B     HK210                                                            
                                                                                
HK200    MVC   R1NME1,1(R4)        REPLACE WITH GROUP NAME                      
                                                                                
HK250    CLI   QOPT7,C'2'                                                       
         BNE   XIT                                                              
         L     R1,COUNT2                                                        
         AHI   R1,1                                                             
         CHI   R1,MAXDUMP                                                       
         BH    XIT                                                              
         ST    R1,COUNT2                                                        
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(10,=C'SHOOK AF=3'),(R7),C'DUMP',(R0),      X        
               =C'2D',(C'P',PRINT)                                              
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------*              
*        SORTHOOK                                                               
*----------------------------------------------------------------*              
HK300    CLI   HOOKTYPE,CMSRHK                                                  
         BNE   XIT                                                              
         CLI   HOOKNUM,1                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   QOPT6,C'3'                                                       
         BNE   HK305                                                            
         L     R1,COUNT3                                                        
         AHI   R1,1                                                             
         CHI   R1,MAXDUMP                                                       
         BH    HK305                                                            
         ST    R1,COUNT3                                                        
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(10,=C'SORTHOOK=4'),(R7),C'DUMP',(R0),      X        
               =C'2D',(C'P',PRINT)                                              
                                                                                
HK305    MVI   R1CDE1,C'1'                                                      
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DS    A                                                                
COUNT1   DC    F'0'                                                             
COUNT2   DC    F'0'                                                             
COUNT3   DC    F'0'                                                             
MAXDUMP  EQU   10                                                               
HOOKSW   DC    C'N'                                                             
                                                                                
*                                                                               
*  FILTER 4 GROUPS                                                              
*                                                                               
F4GROUP  DS    0C                                                               
F4GRP2   DC    C'2',AL1(F4GRP2X-F4GRP2),C'ABCDEFGHIJ'                           
F4GRP2X  EQU   *                                                                
*                                                                               
F4GRP3   DC    C'3',AL1(F4GRP3X-F4GRP3),C'K'                                    
F4GRP3X  EQU   *                                                                
*                                                                               
F4GRP4   DC    C'4',AL1(F4GRP4X-F4GRP4),C'LMN'                                  
F4GRP4X  EQU   *                                                                
*                                                                               
F4GRP5   DC    C'5',AL1(F4GRP5X-F4GRP5),C'OP'                                   
F4GRP5X  EQU   *                                                                
*                                                                               
F4GRP6   DC    C'6',AL1(F4GRP6X-F4GRP6),C'QRS'                                  
F4GRP6X  EQU   *                                                                
         DC    X'00'                                                            
*                                                                               
*  FILTER 5 GROUPS                                                              
*                                                                               
F5GROUP  DS    0C                                                               
F5GRP1   DC    C'1',AL1(F5GRP1X-F5GRP1),C'ABCG6'                                
F5GRP1X  EQU   *                                                                
*                                                                               
F5GRP2   DC    C'2',AL1(F5GRP2X-F5GRP2),C'DEFHIJK58'                            
F5GRP2X  EQU   *                                                                
*                                                                               
F5GRP3   DC    C'3',AL1(F5GRP3X-F5GRP3),C'LMNOPRZ'                              
F5GRP3X  EQU   *                                                                
*                                                                               
F5GRP4   DC    C'4',AL1(F5GRP4X-F5GRP4),C'QSTUVWXY0$'                           
F5GRP4X  EQU   *                                                                
*                                                                               
F5GRP5   DC    C'5',AL1(F5GRP5X-F5GRP5),C'1'                                    
F5GRP5X  EQU   *                                                                
*                                                                               
F5GRP6   DC    C'6',AL1(F5GRP6X-F5GRP6),C'23'                                   
F5GRP6X  EQU   *                                                                
*                                                                               
F5GRP7   DC    C'7',AL1(F5GRP7X-F5GRP7),C'9'                                    
F5GRP7X  EQU   *                                                                
*                                                                               
F5GRP8   DC    C'8',AL1(F5GRP8X-F5GRP8),C'7'                                    
F5GRP8X  EQU   *                                                                
*                                                                               
F5GRP9   DC    C'9',AL1(F5GRP9X-F5GRP9),C'4'                                    
F5GRP9X  EQU   *                                                                
*                                                                               
F5GRPA   DC    C'A',AL1(F5GRPAX-F5GRPA),C'#'                                    
F5GRPAX  EQU   *                                                                
         DC    X'00'                                                            
*                                                                               
* GROUP NAMES                                                                   
*                                                                               
GRPTAB   DS    0C                                                               
         DC    CL1'0',CL36'NOT-GROUPED'                                         
GRPLEN   EQU   *-GRPTAB                                                         
         DC    CL1'1',CL36'ATLANTA - ARTHUR KENNEDY'                            
         DC    CL1'2',CL36'LOS ANGELES - JOHN BARNES'                           
         DC    CL1'3',CL36'CHICAGO - SUSAN ROWE'                                
         DC    CL1'4',CL36'NEW YORK - CHARLIE RUTT'                             
         DC    CL1'5',CL36'DALLAS - KAMELA YATES'                               
         DC    CL1'6',CL36'MEMPHIS - ARTHUR KENNEDY'                            
         DC    CL1'7',CL36'NEW BUSINESS - CHARLIE RUTMAN'                       
         DC    CL1'8',CL36'CARAT TRADE - JOHN KEANNA'                           
         DC    CL1'9',CL36'INTERCOMPANY - JOHN KEANNA'                          
         DC    CL1'A',CL36'CARAT IMS - CHARLIE RUTMAN'                          
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        WORKING STORAGE                                                        
*----------------------------------------------------------------*              
R1RECD   DSECT                                                                  
R1ROW1   DS    XL2                                                              
R1CDE1   DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CDE2   DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CDE3   DS    XL14                                                             
R1ROW4   DS    XL2                                                              
R1CDE4   DS    XL14                                                             
R1ROW5   DS    XL2                                                              
R1CDE5   DS    XL14                                                             
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
R1TYPE   DS    XL2                                                              
*                                                                               
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
R1NME4   DS    CL36                                                             
R1NME5   DS    CL36                                                             
*                                                                               
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1COL5   DS    PL8                                                              
R1COL6   DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
                                                                                
FGROUP   DSECT                                                                  
FGROUP#  DS    CL1                                                              
FGRLN    DS    AL1                                                              
FGRDATA  DS    0CL1                                                             
ACHFUBA  CSECT                                                                  
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073APGHFIUBA 02/23/04'                                      
         END                                                                    
