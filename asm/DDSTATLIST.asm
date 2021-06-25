*          DATA SET DDSTATLIST AT LEVEL 005 AS OF 05/01/02                      
*PHASE STATLIST,*,NOAUTO                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE FATAB                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'STATPRNT - PRINT SELECTED ADRFILE STATISTICS'                   
STATPRNT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**STATPR,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(STXITER),P1,A(DUMPLIST)                                       
*                                                                               
         OPEN  (ADRIN,INPUT)                                                    
         MVC   MID1(103),INHEAD                                                 
         MVC   TITLE(28),=CL28'STATPRNT - FRED ROE NET DATA'                    
         B     INPUT                                                            
*                                                                               
INHEAD   DC    C'  LINE TRM  SYSTEM   PROGRAM  TASK    SIN     START   X        
                 END     DURTN  CPU TM    I/O  STTM(TU)  NDTM(TU)'              
         EJECT                                                                  
INPUT    GET   ADRIN,SORTREC                                                    
*                                                                               
         LA    R3,SORTREC                                                       
         USING ADRRECD,R3                                                       
*                                                                               
         CLI   ADRREC,C'$'         SKIP NEW FORMAT RECORDS                      
         BE    INPUT                                                            
         CLC   =C'*VTAM',ADRREC                                                 
         BNE   INPUT2                                                           
* NEED TO SKIP WHOLE BLOCK (49 MORE RECORDS)                                    
         LA    R5,49                                                            
INSKIP   GET   ADRIN,ADRREC                                                     
         BCT   R5,INSKIP                                                        
         B     INPUT                                                            
*                                                                               
INPUT2   OC    ADRREC(38),ADRREC   IGNORE NULL RECORDS                          
         BZ    INPUT                                                            
         CLC   ADRSTTM,=F'1000000' START TIME HOUR 3                            
         BL    INPUT                                                            
         CLC   ADRNDTM,=F'1500000'                                              
         BNH   INPRT                                                            
         B     INPUT                                                            
         SPACE 1                                                                
         EJECT                                                                  
* ROUTINES TO LIST INPUT                                                        
*                                                                               
INPRT    DS    0H                                                               
*                                                                               
         MVC   PLN,ADRLINE                                                      
         MVC   PTRM,ADRADDR                                                     
*                                                                               
         L     R5,=V(SELIST)                                                    
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
*                                                                               
         CLC   SESYS,ADRSYSNO                                                   
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     INPUT                                                            
         MVC   PSYS,SENAME                                                      
*                                                                               
         L     R5,SEPGMS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
         CLC   PGMNUM,ADRPRGNO                                                  
         BE    BP1                                                              
         BXLE  R5,R6,*-10                                                       
         B     INPUT                                                            
         ZIC   R0,ADRPRGNO                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPRG(2),DUB                                                      
         B     *+10                                                             
*                                                                               
BP1      MVC   PPRG,PGMNAME                                                     
*                                                                               
         MVC   PTSK,ADRTASK                                                     
*                                                                               
         L     R0,ADRSIN                                                        
         EDIT  (R0),(6,PSIN)                                                    
*                                                                               
         L     R0,ADRSTTM                                                       
         BAS   RE,CVTIME                                                        
         MVC   PSTTM,WORK+2                                                     
*                                                                               
         L     R0,ADRNDTM                                                       
         BAS   RE,CVTIME                                                        
         MVC   PNDTM,WORK+2                                                     
*                                                                               
         L     R0,ADRNDTM                                                       
         S     R0,ADRSTTM                                                       
         BAS   RE,CVDUR                                                         
         MVC   PDUR,WORK+4                                                      
*                                                                               
         L     R0,ADRCPUTM                                                      
         BAS   RE,CVDUR                                                         
         MVC   PCPU,WORK+5                                                      
*                                                                               
         LH    R0,ADRIOCNT                                                      
         EDIT  (R0),(5,PIO)                                                     
*                                                                               
         GOTO1 =V(HEXOUT),P1,ADRSTTM,PSTTU,4,=C'MIX'                            
*                                                                               
         GOTO1 (RF),(R1),ADRNDTM,PNDTU,4                                        
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     INPUT                                                            
         EJECT                                                                  
* CONVERT DURATION IN HEX TU TO DEC MS                                          
*                                                                               
CVDUR    LA    R1,1000             X 1000 = MS                                  
         MR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CVD   R1,DUB                                                           
         MVC   WORK(10),=X'40202020202020202120'                                
         ED    WORK(10),DUB+3                                                   
         BR    RE                                                               
         SPACE 2                                                                
* CONVERT TIME IN HEX TU TO HH.MM.SS                                            
CVTIME   SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         ST    R0,DUB              SAVE SECONDS                                 
         SR    R0,R0                                                            
         D     R0,=F'60'           GIVES MIN IN R0,HRS IN R1                    
         MH    R1,=H'10000'        SHIFT LEFT                                   
         MH    R0,=H'100'                                                       
         AR    R1,R0                                                            
         A     R1,DUB                                                           
         CVD   R1,DUB                                                           
         MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),DUB+4                                                   
         BR    RE                                                               
         EJECT                                                                  
INPUT4   AP    INCNT,=P'1'                                                      
         B     INPUT                                                            
         SPACE 2                                                                
*                                                                               
ENDX     DS    0H                                                               
         CLOSE (ADRIN)                                                          
         MVC   P+1(20),INMSG                                                    
         OI    INCNT+3,X'0F'                                                    
         UNPK  P+1(6),INCNT                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
*                                                                               
INCNT    DC    PL4'0'                                                           
INMSG    DC    C'999999 INPUT RECORDS'                                          
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(STATPRNT,65000)                                                
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
DUB      DS    D                                                                
TABADDR  DS    A                                                                
SAVER1   DS    A                                                                
TIME     DC    F'0'                                                             
*                                                                               
SORTKEY  DS    CL16                                                             
         ORG   SORTKEY                                                          
SRTOVSYS DS    CL1                                                              
SRTPRGNO DS    CL1                                                              
SRTSYSNO DS    CL1                                                              
SRTLINE  DS    CL8                                                              
SRTSTTM  DS    CL4                                                              
         DS    CL1                 SPARE                                        
SORTREC  DS    CL38                                                             
*                                                                               
WORK     DS    CL24                                                             
TASKBIT  DC    X'00'                                                            
UPSI     DC    X'00'                                                            
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
ADRIN    DCB   DDNAME=ADRIN,DSORG=PS,MACRF=(GM),EODAD=ENDX,            X        
               RECFM=FB,LRECL=38                                                
         EJECT                                                                  
* DDSADRREC                                                                     
       ++INCLUDE FAADRREC                                                       
         EJECT                                                                  
* FAPGMLST                                                                      
       ++INCLUDE FAPGMLST                                                       
         SPACE 2                                                                
* FASELIST                                                                      
       ++INCLUDE FASELIST                                                       
         EJECT                                                                  
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* DSECT FOR INPUT PRINT LINE                                                    
         ORG   P                                                                
*                                                                               
         DS    2C                                                               
PLN      DS    CL4                                                              
         DS    C                                                                
PTRM     DS    CL4                                                              
         DS    C                                                                
PSYS     DS    CL7                                                              
         DS    2C                                                               
PPRG     DS    CL7                                                              
         DS    4C                                                               
PTSK     DS    C                                                                
         DS    3C                                                               
PSIN     DS    CL6                                                              
         DS    2C                                                               
PSTTM    DS    CL8                                                              
         DS    2C                                                               
PNDTM    DS    CL8                                                              
         DS    1C                                                               
PDUR     DS    CL6                                                              
         DS    3C                                                               
PCPU     DS    CL5                                                              
         DS    2C                                                               
PIO      DS    CL5                                                              
         DS    2C                                                               
PSTTU    DS    CL8                                                              
         DS    2C                                                               
PNDTU    DS    CL8                                                              
         EJECT                                                                  
* DSECT FOR OUTPUT PRINT LINE                                                   
*                                                                               
         ORG   P                                                                
         DS    C                                                                
PTIME    DS    CL10                                                             
         DS    2C                                                               
PTASK1   DS    C                                                                
         DS    2C                                                               
PTASK2   DS    C                                                                
         DS    2C                                                               
PTASK3   DS    C                                                                
         DS    2C                                                               
PTIMETU  DS    CL8                                                              
 END                                                                            
