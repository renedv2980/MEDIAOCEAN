*          DATA SET SCHOCOPY   AT LEVEL 115 AS OF 11/06/00                      
*PHASE SCHOCOPY,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
         TITLE 'EXTRACT CERTAIN RECORD TYPES FROM A DEMO TAPE'                  
SCHOCOPY CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SCHOCOPY,VREGSAVE                                              
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         OPEN  (FILIN1,(INPUT))                                                 
         OPEN  (FILOUT,(OUTPUT))                                                
         XC    INCNT,INCNT                                                      
         XC    OUTCNT,OUTCNT                                                    
         LA    RE,IOAREA                                                        
         LH    R1,=Y(RECLENQ)                                                   
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
         SR    R3,R3                                                            
*                                                                               
SORT2    DS    0H                                                               
         GET   FILIN1,IOAREA       READ IN UNIVERSE RECD                        
         LA    R2,IOAREA                                                        
         USING MIREC,R2                                                         
*                                                                               
SORT3    DS    0H                                                               
SORT3A   DS    0H                                                               
         CLC   MITSEQ,=C'01'                                                    
         BE    SORT2                                                            
*                                                                               
         CLC   MITSEQ,=C'02'                                                    
         BE    SORT2                                                            
*                                                                               
         CLC   MITSEQ,=C'03'                                                    
         BE    SORT2                                                            
*                                                                               
         CLC   MITSEQ,=C'04'                                                    
         BNE   SORT4                                                            
         CLC   MITTYPE(3),=C'TEL'                                               
         BNE   SORT2                                                            
         CLC   MITPRG,=C'0000108107'                                            
         BE    COPY                                                             
         B     SORT2                                                            
*                                                                               
SORT4    CLC   MITSEQ,=C'05'                                                    
         BE    SORT2                                                            
*                                                                               
COPY     DS    0H                                                               
         PUT   FILOUT,(R2)         KEEP THIS RECD                               
         B     SORT2                                                            
*                                                                               
         DS    0H                                                               
ENDCPY   CLOSE (FILIN1,)                                                        
         CLOSE FILOUT                                                           
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
STXTAB   DS    0H                                                               
         DC    A(SCHOCOPY)                                                      
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
VREGSAVE DC    V(REGSAVE)                                                       
         SPACE 2                                                                
FILIN1   DCB   DDNAME=FILIN,DSORG=PS,RECFM=FB,MACRF=(GM),              X        
               EODAD=ENDCPY,LRECL=0401,BLKSIZE=16842                            
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               LRECL=0401,BLKSIZE=16842                                         
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,114,A),FORMAT=BI,WORK=1 '                    
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=401'                                   
*                                                                               
SVFLAG   DS    XL1                                                              
SVTYPE   DS    CL6                                                              
*                                                                               
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
REL      DS    X                                                                
CHAR     DS    C                                                                
FILE     DS    C                                                                
LPDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
STDATE   DS    CL6                                                              
AREC     DS    F                                                                
         DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
LASTREC  DS    CL(RECLENQ)                                                      
         EJECT                                                                  
       ++INCLUDE DENTHID                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115SCHOCOPY  11/06/00'                                      
         END                                                                    
