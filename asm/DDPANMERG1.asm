*          DATA SET DDPANMERG1 AT LEVEL 002 AS OF 05/24/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE PANMER1A                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'MERGE ONE OR TWO SOURCE MODULES INTO A COMMON BASE'             
***********************************************************************         
* THIS PROGRAM IS INVOKED BY THE =P.12 CLIST (JCPVMRGE).                        
* IT WAS ADAPTED BY A PROGRAM ORIGINALLY WRITTEN BY JOHN MCCONNELL.             
* A REQUIRED ONE-BYTE PARAMETER IS PASSED BY THE CLIST:                         
*  C'N' MEANS THERE IS ONE NEW SOURCE TO MERGE WITH THE BASE VERSION.           
*  C'Y' THERE IS A SECOND FILE (ALONG WITH THE FIRST) TO MERGE WITH             
*        THE BASE VERSION.                                                      
***********************************************************************         
         PRINT NOGEN                                                            
PANMER1  CSECT                                                                  
         NBASE 0,PANMER1,=V(REGSAVE)                                            
*                                                                               
         LR    R1,RC                                                            
         SHI   R1,4                                                             
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM) FROM CLIST                           
         CLC   0(2,R1),=H'1'       L'PARM STRING                                
         BNL   *+6                                                              
         DC    H'0'                AT LEAST ONE BYTE PARM MUST BE THERE         
         MVC   FILE2FLG,2(R1)      Y/N                                          
         MVI   EOFSW,0                                                          
*                                                                               
         OPEN  (COMPLIST,OUTPUT)                                                
         OPEN  DETAIL1                                                          
         CLI   FILE2FLG,C'Y'       MERGING 2 FILES WITH BASE?                   
         BNE   GETINREC                                                         
         OPEN  DETAIL2             YES                                          
         B     GETFROM1                                                         
*                                                                               
REC1     USING DETRECD,DET1REC                                                  
REC2     USING DETRECD,DET2REC                                                  
*                                                                               
* HANDLE MERGING OF ONE FILE WITH A BASE VERSION                                
*                                                                               
GETINREC DS    0H                                                               
         GET   DETAIL1,DET1REC                                                  
*                                                                               
         MVC   CRDATA,REC1.DDATA                                                
         CLC   REC1.DACT,DELETE                                                 
         BE    PROCREC                                                          
         CLC   REC1.DACT,EQUAL                                                  
         BE    PROCREC                                                          
         CLC   REC1.DACT,INSERT                                                 
         BNE   GETINREC            IGNORE: NOT A SOURCE STATEMENT               
*                                                                               
         MVC   CRACT,INSERT+1      INSERTION                                    
         MVC   CRSEQ1,=C'      '                                                
         MVC   CRSEQ2,REC1.DSEQ2                                                
         B     PUTREC                                                           
*                                                                               
PROCREC  DS    0H                                                               
         MVC   CRACT,EQUAL                                                      
         CLC   REC1.DACT,EQUAL                                                  
         BE    *+10                LINE IS UNCHANGED                            
         MVC   CRACT,DELETE+1      IT'S A DELETION                              
         MVC   CRSEQ1,REC1.DSEQ1                                                
         MVC   CRSEQ2,REC1.DSEQ2                                                
*                                                                               
PUTREC   DS    0H                                                               
         PUT   COMPLIST,COMPREC                                                 
         B     GETINREC                                                         
         EJECT                                                                  
*                                                                               
* HANDLE MERGING OF TWO FILES WITH A BASE VERSION                               
*                                                                               
GETFROM1 DS    0H                                                               
         TM    EOFSW,EOF1FLAG                                                   
         BNZ   GETFROM2                                                         
*                                                                               
         GET   DETAIL1,DET1REC                                                  
         B     CHECK1                                                           
*                                                                               
EOF1     DS    0H                                                               
         CLI   FILE2FLG,C'Y'       MERGING 2 FILES WITH BASE?                   
         BNE   CLOSE               NO                                           
*                                                                               
         OI    EOFSW,EOF1FLAG      REMEMBER: EOF REACHED ON FILE 1              
         CLI   EOFSW,EOJFLAG                                                    
         BE    CLOSE                                                            
         B     GETFROM2                                                         
*                                                                               
CHECK1   DS    0H                                                               
         CLC   REC1.DACT,DELETE                                                 
         BE    GETFROM2                                                         
         CLC   REC1.DACT,EQUAL                                                  
         BE    GETFROM2                                                         
         CLC   REC1.DACT,INSERT                                                 
         BNE   GETFROM1            IGNORE: NOT A SOURCE STATEMENT               
*                                                                               
         MVC   COMPREC,BLANKS      INSERT FROM FILE 1                           
         MVC   CRACT1,INS5                                                      
         MVC   CRDATA,REC1.DDATA                                                
         MVC   CRSEQ1,BLANKS                                                    
         MVC   CRSEQ2,REC1.DSEQ2                                                
         MVC   CRSEQ3,BLANKS                                                    
         PUT   COMPLIST,COMPREC                                                 
         B     GETFROM1                                                         
*                                                                               
GETFROM2 DS    0H                                                               
         TM    EOFSW,EOF2FLAG                                                   
         BNZ   GETFROM1                                                         
*                                                                               
         GET   DETAIL2,DET2REC                                                  
         B     CHECK2                                                           
*                                                                               
EOF2     DS    0H                                                               
         OI    EOFSW,EOF2FLAG      REMEMBER: EOF REACHED ON FILE 2              
         CLI   EOFSW,EOJFLAG                                                    
         BE    CLOSE                                                            
         B     GETFROM1                                                         
*                                                                               
CHECK2   DS    0H                                                               
         MVC   COMPREC,BLANKS                                                   
         CLC   REC2.DACT,DELETE                                                 
         BE    CHKDIFF                                                          
         CLC   REC2.DACT,EQUAL                                                  
         BE    CHKDIFF                                                          
         CLC   REC2.DACT,INSERT                                                 
         BNE   GETFROM2            IGNORE: NOT A SOURCE STATEMENT               
*                                                                               
         MVC   CRACT2,INS5         INSERTION FROM FILE 2                        
         MVC   CRDATA,REC2.DDATA                                                
         MVC   CRSEQ1,BLANKS                                                    
         MVC   CRSEQ2,BLANKS                                                    
         MVC   CRSEQ3,REC2.DSEQ2                                                
         PUT   COMPLIST,COMPREC                                                 
         B     GETFROM2                                                         
*                                                                               
CHKDIFF  DS    0H                                                               
         CLC   REC1.DSEQ1,REC2.DSEQ1                                            
         BE    *+6                                                              
         DC    H'0'                ORIGINAL SEQUENCE NUMBER MISMATCH!           
*                                                                               
CHKREC1  DS    0H                                                               
         MVC   CRACT1,EQUAL                                                     
         MVC   CRACT2,EQUAL                                                     
         CLC   REC1.DACT,EQUAL                                                  
         BE    *+10                LINE IS UNCHANGED                            
         MVC   CRACT1,DEL5         IT'S A DELETION FROM FILE 1                  
*                                                                               
         CLC   REC2.DACT,EQUAL                                                  
         BE    *+10                LINE IS UNCHANGED                            
         MVC   CRACT2,DEL5         IT'S A DELETION FROM FILE 2                  
*                                                                               
         MVC   CRDATA,REC1.DDATA                                                
         MVC   CRSEQ1,REC1.DSEQ1                                                
         MVC   CRSEQ2,REC1.DSEQ2                                                
         MVC   CRSEQ3,REC2.DSEQ2                                                
         PUT   COMPLIST,COMPREC                                                 
         B     GETFROM1                                                         
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE COMPLIST                                                         
         CLOSE DETAIL1                                                          
         CLI   FILE2FLG,C'Y'       MERGING 2 FILES WITH BASE?                   
         BNE   XBASE               NO                                           
         CLOSE DETAIL2                                                          
*                                                                               
XBASE    DS    0H                                                               
         XBASE                                                                  
*                                                                               
         DROP  REC1,REC2                                                        
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DET1REC  DS    CL133                                                            
DET2REC  DS    CL133                                                            
*                                                                               
COMPREC  DC    CL133' '                                                         
         ORG   COMPREC                                                          
CRACT    DS    CL10                                                             
         ORG   CRACT                                                            
CRACT1   DS    CL5                                                              
CRACT2   DS    CL5                                                              
         DS    CL1                                                              
CRDATA   DS    CL80                                                             
         DS    CL1                                                              
CRSEQ1   DS    CL6                                                              
         DS    CL1                                                              
CRSEQ2   DS    CL6                                                              
         DS    CL1                                                              
CRSEQ3   DS    CL6                                                              
         DS    CL1                                                              
         ORG                                                                    
*                                                                               
EQUAL    DC    CL12'============'                                               
DELETE   DC    CL12'///DELETE///'                                               
INSERT   DC    CL12'***INSERT***'                                               
INS5     DC    C'*INS*'                                                         
DEL5     DC    C'/DEL/'                                                         
*                                                                               
FILE2FLG DC    C'N'                'Y': MERGE 2 FILES WITH BASE VERSION         
EOFSW    DC    X'00'                                                            
EOF1FLAG EQU   X'F0'                                                            
EOF2FLAG EQU   X'0F'                                                            
EOJFLAG  EQU   X'FF'                                                            
*                                                                               
BLANKS   DC    CL133' '                                                         
*                                                                               
DETAIL1  DCB   DDNAME=DETAIL1,DSORG=PS,RECFM=FBA,LRECL=133,MACRF=GM,   X        
               EODAD=EOF1                                                       
*                                                                               
DETAIL2  DCB   DDNAME=DETAIL2,DSORG=PS,RECFM=FBA,LRECL=133,MACRF=GM,   X        
               EODAD=EOF2                                                       
*                                                                               
COMPLIST DCB   DDNAME=COMPLIST,DSORG=PS,RECFM=FB,LRECL=133,MACRF=PM             
         SPACE 3                                                                
DETRECD  DSECT                                                                  
DETREC   DS    CL133                                                            
         ORG   DETREC+1                                                         
DSEQ1    DS    CL6                                                              
         ORG   DETREC+8                                                         
DPLDEL   DS    CL10                                                             
         ORG   DETREC+10                                                        
DSEQ2    DS    CL6                                                              
         ORG   DETREC+19                                                        
DACT     DS    CL12                                                             
         ORG   DETREC+33                                                        
DDATA    DS    CL12                                                             
         ORG   DETREC+111                                                       
DMARK    DS    CL1                                                              
         ORG   DETREC+113                                                       
DSEQ3    DS    CL6                                                              
         ORG                                                                    
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDPANMERG105/24/13'                                      
         END                                                                    
