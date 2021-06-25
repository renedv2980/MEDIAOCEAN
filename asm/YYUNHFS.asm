*          DATA SET YYUNHFS    AT LEVEL 001 AS OF 03/30/01                      
*PHASE YYUNHFSA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DDINFO                                                                 
*                                                                               
* This is a test program :  given the "FILE0" ddname in the JCL,                
* call ddinfo to check if this is a HFS file with FILEDATA=TEXT,                
* if so, read the dataset as variable-block record and print each               
* record to the SYSPRINT.                                                       
*                                                                               
         TITLE ' YYUNRHFS'                                                      
YYUNRHFS CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*YYUNRHFS,=A(R13CHAIN)                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         MVC   TUKEY,=X'C017'      TEXT UNIT FOR HFS FILENAME                   
         GOTO1 =V(DDINFO),DMCB,(5,DDNAME),TUKEY,0                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,DMCB+8                                                        
         CLC   =C'/u/',0(RE)       HFS PATHNAME?                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TUKEY,=X'C01D'      TEXT UNIT FOR FILEDATA                       
         GOTO1 =V(DDINFO),DMCB,(5,DDNAME),TUKEY,0                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,DMCB+8                                                        
         ICM   RF,1,DMCB+8                                                      
         BNZ   *+6                                                              
         DC    H'0'                NO FILEDATA PARAM                            
*                                                                               
         CLI   0(RE),X'40'                                                      
         BE    *+6                                                              
         DC    H'0'                FILEDATA != TEXT                             
*                                                                               
         LA    R1,FILE             R1=INPUT DCB                                 
         USING IHADCB,R1                                                        
         MVI   DCBRECFM,DCBRECV+DCBRECBR      RECFM=VB                          
         MVC   DCBBLKSI,=H'25500'             BLKSIZE=25500                     
         MVC   DCBLRECL,=H'255'               LRECL=255                         
         DROP  R1                                                               
*                                                                               
*                                                                               
         OPEN  (FILE,INPUT)                                                     
H        GET   FILE,BUF                                                         
         LA    R1,FILE             R1=INPUT DCB                                 
         USING IHADCB,R1                                                        
         CLI   DCBRECFM,DCBRECV+DCBRECBR      RECFM=VB                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         MVC   P,BUF                                                            
         GOTO1 =V(PRINTER)                                                      
         B     H                                                                
*                                                                               
EOF      DS    0H                                                               
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
*                                                                               
DMCB     DS    6F                                                               
DDNAME   DC    C'FILE0'                                                         
TUKEY    DC    X'C017'                                                          
*                                                                               
FILE     DCB   DDNAME=FILE0,DSORG=PS,MACRF=(GM),EODAD=EOF                       
         DC    C'*BUFFER*'                                                      
BUF      DS    CL300                                                            
         DC    C'*BUFFER*'                                                      
         DS    400C                                                             
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001YYUNHFS   03/30/01'                                      
         END                                                                    
