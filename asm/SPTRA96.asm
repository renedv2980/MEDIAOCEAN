*          DATA SET SPTRA96    AT LEVEL 010 AS OF 05/01/02                      
*PHASE T21696A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'T21696 - DEALER INST AUTO-REQUEST GENERATOR'                    
***********************************************************************         
*                                                                     *         
*  LEV  6-7  MAR25/88 FIX SHIPPING TABLE ERROR                        *         
*  LEV  8    SEP07/89 FIX SHIPPING TABLE ERROR                        *         
*  LEV  8    NOV13/89 FIX MASTER BUG WITH /* FOR NOW                  *         
*                                                                     *         
***********************************************************************         
T21696   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21696,RR=R8                                                   
         LA    R7,2048(R7)                                                      
         LA    R7,2048(R7)                                                      
         USING T21696+4096,R7                                                   
         ST    R8,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
*                                                                               
*                                                                               
         L     RE,=V(SORTER)                                                    
         A     RE,RELO                                                          
         ST    RE,VSORTER                                                       
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
*                                                                               
         LA    R2,TR15FIL                                                       
         OPEN  ((R2),INPUT)                                                     
*                                                                               
         LA    RE,ENDFILE          RELOCATE EOD ADDRESS                         
         STCM  RE,7,TR15FIL+33                                                  
         SPACE                                                                  
* ALWAYS NEED TO OPEN AND CLOSE REQUEST FILES *                                 
         SPACE                                                                  
         LA    R2,DLRQFIL                                                       
         OPEN  ((R2),OUTPUT)                                                    
*                                                                               
         LA    R2,SHPQFIL                                                       
         OPEN  ((R2),OUTPUT)                                                    
*                                                                               
         LA    R7,ELEM                                                          
         USING XRECD,R7                                                         
*                                                                               
         B     GETFILE                                                          
         SPACE 2                                                                
GETFILE  LA    R2,TR15FIL                                                       
         GET   (R2),(R7)                                                        
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',(R7)                                        
*                                                                               
         B     GETFILE                                                          
         SPACE 2                                                                
ENDFILE  DS    0H                                                               
         LA    R2,TR15FIL                                                       
         CLOSE ((2),)                                                           
*                                                                               
         LA    R7,ELEM             POINT TO DUMMY AREA                          
         XC    ELEM,ELEM           CLEAR SO NO MATCH                            
         EJECT                                                                  
         USING XRECD,R7                                                         
*                                                                               
GETSORT  MVC   ELEM(L'XREC),0(R7)          SAVE PREVIOUS RECORD                 
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R7,15,4(R1)         TEST EOF                                     
         BZ    ENDSORT                                                          
*                                                                               
         CLC   ELEM(L'XREC),0(R7)  TEST DUPLICATE REQUEST                       
         BE    GETSORT             YES - IGNORE                                 
         SPACE                                                                  
         CLI   XTYPE,C'R'          TEST DEALER RECAP REQ                        
         BE    DLR                                                              
         CLI   XTYPE,C'S'          TEST SHIPPING ORDER REQ                      
         BE    SHP                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
ENDSORT  DS    0H                                                               
*                                                                               
         MVC   DREQ(2),=C'/*'                                                   
         MVC   DREQ+2(78),SPACES                                                
         LA    R0,DREQ                                                          
         LA    R1,DLRQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         LA    R2,DLRQFIL                                                       
         CLOSE ((2))                                                            
*                                                                               
         LA    R0,DREQ                                                          
         LA    R1,SHPQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         LA    R2,SHPQFIL                                                       
         CLOSE ((2))                                                            
*                                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
* DEALER RECAP REQUESTS *                                                       
         SPACE                                                                  
DLR      AP    REQNUM,=P'1'                                                     
         OI    REQNUM+3,X'0F'                                                   
*                                                                               
         MVC   DREQ,SPACES                                                      
         MVC   DREQ(2),=C'TC'                                                   
         MVC   DREQ+2(2),AGENCY                                                 
         UNPK  DREQ+5(6),REQNUM                                                 
*                                                                               
         LA    R4,DREQ+12                                                       
         MVC   0(19,R4),=C'0204SPOT 0305RECAP '                                 
         LA    R4,19(R4)                                                        
         LA    R5,DREQ1TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,DREQ                                                          
         LA    R1,DLRQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   DREQ+12(68),SPACES                                               
*                                                                               
         LA    R4,DREQ+12                                                       
         LA    R5,DREQ2TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,DREQ                                                          
         LA    R1,DLRQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   DREQ+12(68),SPACES                                               
         LA    R4,DREQ+12                                                       
*                                                                               
         MVC   0(4,R4),=C'1317'                                                 
         GOTO1 DATCON,DMCB,XFLTST,(5,4(R4))                                     
         MVI   12(R4),C'-'                                                      
         GOTO1 (RF),(R1),XFLTEND,(5,13(R4))                                     
*                                                                               
         MVI   21(R4),C'*'                                                      
*                                                                               
         LA    R0,DREQ                                                          
         LA    R1,DLRQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         B     GETSORT                                                          
*                                                                               
DREQ1TAB DC    AL1(5,8),AL4(XWHEN-XRECD)                                        
         DC    X'FF'                                                            
*                                                                               
DREQ2TAB DC    AL1(9,1),AL4(XMED-XRECD)                                         
         DC    AL1(10,3),AL4(XCLT-XRECD)                                        
         DC    AL1(11,3),AL4(XPRD-XRECD)                                        
         DC    AL1(12,4),AL4(XMKT-XRECD)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* SHIPPING ORDER REQUESTS *                                                     
         SPACE                                                                  
SHP      DS    0H                                                               
         AP    REQNUM,=P'1'                                                     
         OI    REQNUM+3,X'0F'                                                   
*                                                                               
         MVC   SREQ,SPACES                                                      
         MVC   SREQ(2),=C'TO'                                                   
         MVC   SREQ+2(2),AGENCY                                                 
         UNPK  SREQ+5(6),REQNUM                                                 
*                                                                               
         LA    R4,SREQ+12                                                       
         MVC   0(17,R4),=C'0204SHIP 0303GEN '                                   
         LA    R4,17(R4)                                                        
         LA    R5,SREQ1TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,SREQ                                                          
         LA    R1,SHPQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   SREQ+12(68),SPACES                                               
*                                                                               
         LA    R4,SREQ+12                                                       
         LA    R5,SREQ2TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         L     R4,NEXTCOL                                                       
         CLI   XRERUN,C' '         TEST RERUN DATE PRESENT                      
         BNH   SHP10                                                            
         MVC   0(22,R4),=CL22'1418RERUN=JAN01/00,T/A'                           
         GOTO1 DATCON,DMCB,XRERUN,(5,10(R4))                                    
         LA    R4,23(,R4)          BUMP TO INCLUDE SPACE                        
         B     SHP20                                                            
*                                                                               
SHP10    MVC   0(7,R4),=C'1403T/A'                                              
         LA    R4,8(,R4)           BUMP TO INCLUDE SPACE                        
*                                                                               
SHP20    BCTR  R4,0                                                             
         MVI   0(R4),C'*'          SET EOD FLAG                                 
*                                                                               
         LA    R0,SREQ                                                          
         LA    R1,SHPQFIL                                                       
         PUT   (1),(0)                                                          
         B     GETSORT                                                          
         SPACE 2                                                                
SREQ1TAB DC    AL1(5,8),AL4(XWHEN-XRECD)                                        
         DC    X'FF'                                                            
*                                                                               
SREQ2TAB DC    AL1(9,1),AL4(XMED-XRECD)                                         
         DC    AL1(10,3),AL4(XCLT-XRECD)                                        
         DC    AL1(13,12),AL4(XQUESTOR-XRECD)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*******************************************                                     
* SUBROUTINE TO MOVE DATA TO REQUEST CARD *                                     
* R4 POINTS TO NEXT OUTPUT ADDRESS        *                                     
* R5 POINTS TO DATA TABLE                 *                                     
*******************************************                                     
         SPACE                                                                  
SETREQ   NTR1                                                                   
*                                                                               
SETREQ0  ZIC   R0,1(R5)            GET MAXIMUM DATA LENGTH                      
         ICM   R1,15,2(R5)         GET DATA DSPL IN XREC                        
         LA    R1,XREC(R1)                                                      
         STM   R0,R1,DUB           SAVE MAX LEN/ADDRESS                         
*                                                                               
         AR    R1,R0               POINT PAST END                               
*                                                                               
SETREQ2  BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    SETREQ4                                                          
         BCT   R0,SETREQ2                                                       
         B     SETREQ10                                                         
*                                                                               
SETREQ4  LR    RF,R0                                                            
         BCTR  RF,0                                                             
         L     R1,DUB+4            GET DATA ADDRESS                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R4),0(R1) *EXECUTED*                                         
         SPACE                                                                  
* SET ACTUAL DATA LENGTH                                                        
         SPACE                                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R4),DUB                                                      
         SPACE                                                                  
* SET FIELD NUMBER                                                              
         SPACE                                                                  
         ZIC   R1,0(R5)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
*                                                                               
         AR    R4,R0               ADD LENGTH OF EXECUTED MOVE                  
         LA    R4,5(R4)            BUMP FOR FIELD NUM/FIELD LEN/SPACE           
*                                                                               
SETREQ10 LA    R5,6(R5)            NEXT TABLE ENTRY                             
         CLI   0(R5),X'FF'                                                      
         BNE   SETREQ0                                                          
         ST    R4,NEXTCOL          SET NEXT FIELD START ADDR                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
REQNUM   DC    PL4'0'                                                           
VSORTER  DC    A(0)                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,64,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=64'                                    
SREQ     DS    CL80                                                             
DREQ     DS    CL80                                                             
NEXTCOL  DS    A                                                                
         SPACE                                                                  
         LTORG                                                                  
         SPACE                                                                  
         DS    0D                                                               
TR15FIL  DCB   DDNAME=TR15FIL,DSORG=PS,RECFM=FB,LRECL=64,              X        
               BLKSIZE=3200,MACRF=GM,EODAD=ENDFILE                              
*                                                                               
DLRQFIL  DCB   DDNAME=DLRQFIL,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=2000,MACRF=PM                                            
*                                                                               
SHPQFIL  DCB   DDNAME=SHPQFIL,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=2000,MACRF=PM                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         SPACE                                                                  
* DSECT FOR DEALER RECAP AND SHIP GEN AUTO REQUESTS *                           
         SPACE                                                                  
XRECD    DSECT                                                                  
XREC     DS    0XL64                                                            
*                                                                               
XTYPE    DS    CL1                 D=DLR RCP/S=SHP ORDER                        
XQUESTOR DS    CL12                REQUESTOR (FOR SHIP ORDER)                   
*                                  PTN RCP USES 'WHEN' FIELD HERE               
XMED     DS    CL1                 MEDIA                                        
XCLT     DS    CL3                 CLIENT                                       
XPRD     DS    CL7                 PRD1-SLN1                                    
XPRD2    DS    CL7                 PRD2-SLN2                                    
XCOPY    DS    CL1                 COPY CODE                                    
XMKT     DS    CL4                 MARKET (DEALER RECAP ONLY)                   
XRERUN   DS    0CL6                RERUN DATE (SHP ORDER ONLY)                  
XFLTST   DS    CL6                 FLIGHT START                                 
XFLTEND  DS    CL6                 FLIGHT END                                   
XWHEN    DS    CL7                 PRINT WHEN                                   
         DS    CL9                 SPARE                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPTRA96   05/01/02'                                      
         END                                                                    
