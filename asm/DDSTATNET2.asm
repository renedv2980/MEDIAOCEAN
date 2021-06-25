*          DATA SET DDSTATNET2 AT LEVEL 004 AS OF 10/21/87                      
*PHASE STATNET2,*,NOAUTO                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'STATNET2 - PRINT NET2 ADRFILE STATISTICS'                       
STATNET2 CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**STATN2,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         OPEN  (ADRIN,INPUT)                                                    
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         MVC   MID1(25),INHEAD1                                                 
         MVC   MID2(25),INHEAD2                                                 
         MVC   TITLE(17),=C'NET2 TRANSACTIONS'                                  
         SR    R5,R5                                                            
         B     INPUT                                                            
*                                                                               
INHEAD1  DC    C'  LINE ADDR  TRANSACTIONS'                                     
INHEAD2  DC    C'  ---- ----  ------------'                                     
SORTCARD DC    CL80'SORT FIELDS=(1,8,BI,A),WORK=1'                              
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=46'                                    
         EJECT                                                                  
INPUT    GET   ADRIN,SORTREC                                                    
*                                                                               
         LA    R3,SORTREC                                                       
         USING ADRRECD,R3                                                       
*                                                                               
         CLI   ADRREC,C'$'         SKIP NEW FORMAT RECORDS                      
         BE    INPUT                                                            
         OC    ADRREC,ADRREC       IGNORE NULL RECORDS                          
         BZ    INPUT                                                            
         CLI   ADRSYSNO,X'25'      IGNORE ALL BUT NET2                          
         BNE   INPUT                                                            
*                                                                               
         MVC   SORTKEY,ADRLINE     LINE/TERM                                    
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTKEY                                  
         B     INPUT                                                            
         EJECT                                                                  
* ROUTINES TO LIST INPUT                                                        
*                                                                               
INPRT    CLOSE (ADRIN)                                                          
*                                                                               
INPRT2   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)         TEST FINISHED                                
         BZ    ENDX                YES                                          
*                                                                               
         LA    R3,L'SORTKEY(R3)    POINT TO ADRREC                              
         CLC   SVLINEAD,ADRLINE    TEST CHANGE IN LINE/ADDR                     
         BNE   *+12                                                             
         LA    R5,1(R5)                                                         
         B     INPRT2                                                           
*                                                                               
         MVC   PLN,SVLINEAD                                                     
         MVC   PTRM,SVLINEAD+4                                                  
         EDIT  (R5),(8,PTRANS)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   SVLINEAD,ADRLINE    SAVE LINE/ADDR                               
         LA    R5,1                                                             
         B     INPRT2                                                           
         SPACE 3                                                                
ENDX     GOTO1 =V(SORTER),DMCB,=C'END'                                          
         XBASE                                                                  
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
SORTKEY  DS    CL8                                                              
SORTREC  DS    CL38                                                             
WORK     DS    CL24                                                             
SVLINEAD DC    XL8'00'                                                          
         SPACE 5                                                                
         LTORG                                                                  
         SPACE 5                                                                
ADRIN    DCB   DDNAME=ADRIN,DSORG=PS,MACRF=(GM),EODAD=INPRT,           X        
               RECFM=FB,BLKSIZE=1900,LRECL=38                                   
         EJECT                                                                  
       ++INCLUDE FAADRREC                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
* DSECT FOR INPUT PRINT LINE                                                    
*                                                                               
         ORG   P                                                                
         DS    2C                                                               
PLN      DS    CL4                                                              
         DS    C                                                                
PTRM     DS    CL4                                                              
         DS    2C                                                               
PTRANS   DS    CL8                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDSTATNET210/21/87'                                      
         END                                                                    
