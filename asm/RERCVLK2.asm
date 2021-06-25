*          DATA SET RERCVLK2   AT LEVEL 163 AS OF 05/01/02                      
*          DATA SET RERCVLK2   AT LEVEL 161 AS OF 04/28/95                      
*PHASE RELOOKA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE LOADER                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'RERCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
RCVLOOK  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,RCVLOOK,R9,RR=R2                                               
         ST    R2,RELO                                                          
         L     RF,=V(SORTER)                                                    
         A     RF,RELO                                                          
         ST    RF,ASORT                                                         
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCVLOOK+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         GOTO1 ASORT,DMCB,SORTCARD,RECCARD,0                                    
*                                  INITIALIZE SORT                              
         XC    SORTREC,SORTREC                                                  
*                                                                               
         LA    R5,RKEY                                                          
         USING RBUYRECD,R5         SET DSECT FOR BUY RECORD                     
*                                                                               
         B     INIT2                                                            
RELO     DC    A(0)                                                             
ASORT    DS    A                                                                
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,RCVLOOK          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY LOOK PROGRAM'                           
*                                                                               
         B     IN2                                                              
         EJECT                                                                  
*                                                                               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
*                                                                               
         CLI   RKEY,X'0B'          BUY RECORD?                                  
         BNE   IN2                 ONLY ACCEPT BUY RECORDS                      
         ZICM  RF,RKEY+27,2        GET RECORD LENGTH                            
         AR    RF,R5               CALCULATE END OF RECORD                      
         LA    R2,RKEY+34          SET A(01 ELEMENT)                            
IN0020   EQU   *                                                                
         CLI   0(R2),0             END OF RECORD REACHED?                       
         BE    IN2                 YES - GET NEXT RECORD                        
         CLI   1(R2),0             NO  - BAD LENGTH CHARACTER?                  
         BE    IN2                 YES - GET NEXT RECORD                        
         CLI   0(R2),4             BUY COMMENT ELEMENT?                         
         BNE   IN0040              NO                                           
         CLC   =C'CR=',2(R2)       CREDIT ENTRY?                                
         BE    IN2                 YES - GET NEXT RECORD                        
         B     IN0060              NO  - BUMP FOR NEXT ELEMENT                  
IN0040   EQU   *                                                                
         CLI   0(R2),8             SPOTPAK INTERFACE ELEMENT?                   
         BE    IN0080              YES - ACCEPT THIS RECORD                     
IN0060   EQU   *                                                                
         ZIC   RE,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RE               BUMP TO NEXT ELEMENT                         
         CR    R2,RF               CHECK AGAINST EOR                            
         BL    IN0020              GO BACK FOR NEXT ELEMENT                     
         B     IN2                 GO BACK FOR NEXT RECORD                      
IN0080   EQU   *                                                                
*   TEST                                                                        
*        L     RF,RECCTR                                                        
*        LA    RF,1(RF)                                                         
*        ST    RF,RECCTR                                                        
*        CH    RF,=H'30'                                                        
*        BE    OUT0100             FINISHED                                     
*   TEST END                                                                    
*                                                                               
*                                                                               
*   TEST                                                                        
*        CLC   RBUYKCON,=X'65796069'                                            
*        BNE   TEST0010                                                         
*        BAS   RE,PRINTIT                                                       
*        DC    H'0'                                                             
TEST0010 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         MVC   SORTKREP,RBUYKREP   INSERT REP CODE TO SORT                      
         MVC   SORTKCON,RBUYKCON   INSERT CONTRACT NUMBER                       
         MVC   SORTKBUY,RBUYKMLN   INSERT MAIN LINE, MG LINE                    
****     GOTO1 =V(PRNTBL),DMCB,0,SORTREC,C'DUMP',8,=C'1C'                       
         GOTO1 ASORT,DMCB,=C'PUT',SORTREC                                       
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR                                                       
*        BAS    RE,PRINTIT                                                      
         B     IN2                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
OUT0100  EQU     *                                                              
         CLOSE RECVIN                                                           
         MVC   P+1(15),=C'RECOVERY CLOSED'                                      
         GOTO1 =V(PRINTER)                                                      
OUT0120  EQU   *                                                                
         GOTO1 ASORT,DMCB,=C'GET'                                               
         L     R6,DMCB+4                                                        
         LTR   R6,R6               END OF FILE?                                 
         BZ    EOJ                 YES - END JOB                                
         MVC   SORTREC,0(R6)                                                    
         MVC   P+1(2),SORTKREP                                                  
         GOTO1 =V(HEXOUT),DMCB,SORTKCON,NEWCON,4,=C'TOG'                        
         BAS   RE,CONVCON                                                       
         GOTO1 =V(HEXOUT),DMCB,SORTKBUY,P+20,2,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
         B     OUT0120                                                          
*                                                                               
EOJ      EQU   *                                                                
         GOTO1 ASORT,DMCB,=C'END'                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
NEWCON   DS    CL8                                                              
*                                                                               
CONVCON  NTR1                                                                   
         LA    R1,NEWCON+7         LAST CHAR OF CON#                            
         LA    R2,P+5              FIRST PRINT POSITION                         
         LA    R3,8                LOOP CONTROL                                 
CONV0020 EQU   *                                                                
         ZIC   RF,0(R1)            PLACE CHAR IN REG                            
         SLL   RF,28               DROP ZONE BITS                               
         SRL   RF,28                                                            
         LA    RE,9                                                             
         SR    RE,RF               GET 9'S COMPLEMENT                           
         EDIT  (RE),(1,(R2)),ZERO=NOBLANK                                       
         LA    R2,1(R2)            BUMP TO NEXT PRINT POSITION                  
         BCTR  R1,0                BACK UP 1 CHAR POSITION                      
         BCT   R3,CONV0020         GO DO NEXT POSITION                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
PRINTIT  NTR1                                                                   
*        LA    RE,RCVREC                                                        
*        LA    R3,36                                                            
*        GOTO1 =V(PRNTBL),DMCB,0,RCVREC,C'DUMP',(R3),=C'1D'                     
*                                                                               
         LA    RE,RCVREC                                                        
         LH    R3,0(RE)                                                         
*        SH    R3,=H'24'                                                        
*        GOTO1 =V(PRNTBL),DMCB,0,RCVREC+4+24,C'DUMP',(R3),=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,0,RCVREC,C'DUMP',(R3),=C'1D'                     
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN2  DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               BLKSIZE=2104,MACRF=GM,EODAD=OUT0100                              
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2024,             X        
               BLKSIZE=2028,MACRF=GM,EODAD=OUT0100                              
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=02024,           X        
               BLKSIZE=2028,MACRF=PM                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTREC  DS    0CL8                                                             
SORTKEY  DS    0CL8                                                             
SORTKREP DS    CL2   +00                    REP CODE                            
SORTKCON DS    CL4   +02                    CONTRACT NUMBER                     
SORTKBUY DS    CL2   +06                    BUY NUMBER                          
SORTRECX EQU   *                                                                
         DS    0D                                                               
         DC    CL8'SORTCARD'                                                    
SORTCARD DC    CL80'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=8'                                     
*                                                                               
CLUNPK   DS    F                                                                
STAPACK  DS    F                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
SVPRD    DS    X                                                                
ELCODE   DS    X                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
WORK     DS    XL64                                                             
OUTCNT   DC    PL4'0'                                                           
RECCTR   DS    F                                                                
SORTCTR  DS    F                                                                
*                                                                               
RSORTKEY DS    0XL13                                                            
RSORTAM  DS    XL1                                                              
RSORTCLT DS    XL2                                                              
RSORTMKT DS    XL2                                                              
RSORTSTA DS    XL3                                                              
RSORTPRD DS    XL1                                                              
RSORTEST DS    XL1                                                              
RSORTBUY DS    XL3                                                              
*                                                                               
RSORTSEQ DS    XL3                                                              
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVREC   DC    F'0'                                                             
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL13                                                            
         DS    2100C                                                            
*                                                                               
WORKX    DS    0X                                                               
         EJECT                                                                  
RBUYRECD DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
       ++INCLUDE SPSTAPACKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'163RERCVLK2  05/01/02'                                      
         END                                                                    
