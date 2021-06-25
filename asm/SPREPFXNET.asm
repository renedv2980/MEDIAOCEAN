*          DATA SET SPREPFXNET AT LEVEL 002 AS OF 11/10/96                      
*          DATA SET SPREPFX134 AT LEVEL 022 AS OF 03/28/96                      
*PHASE SPFX02Z                                                                  
         TITLE 'SPFX02 - PRINT X'0D91' RECORDS'                                 
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D91'                                                  
         GOTO1 HIGH                                                             
FX2      GOTO1 SEQ                                                              
         CLC   KEY(2),=X'0D91'                                                  
         BNE   FX10                                                             
         GOTO1 HEXOUT,DMCB,KEY,P,18,=C'TOG'                                     
         GOTO1 REPORT                                                           
         B     FX2                                                              
FX10     GOTO1 AENDREQ                                                          
         OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
FX2      LA    R0,REC-4                                                         
         GET   FILEIN,(R0)                                                      
         CLC   REC+1(4),=C'CK R'                                                
         BE    FX4                                                              
         CLC   REC+1(4),=C'CK T'                                                
         BE    FX4                                                              
         B     FX10                                                             
*                                                                               
FX4      MVC   REQ+4(1),REC+4      MEDIA                                        
         MVC   REQ+5(3),REC+6      CLIENT                                       
         MVC   REQ+11(3),REC+10    PRODUCT                                      
         MVC   REQ+23(3),REC+14    ESTIMATE                                     
         MVC   P(80),REQ                                                        
         GOTO1 REPORT                                                           
         B     FX2                                                              
*                                                                               
FX10     CLC   =C'PAID',REC+1                                                   
         BNE   FX2                                                              
         PUT   FILEOUT,REQ                                                      
         MVC   P(80),REQ                                                        
         GOTO1 REPORT                                                           
         B     FX2                                                              
*                                                                               
REQ      DC    CL80'A2CKTCC    CK ALL ALL  001     ES'                          
         ORG   REQ+61                                                           
         DC    C'P'                                                             
         ORG   REQ+68                                                           
         DC    CL12'COCHON'                                                     
*                                                                               
INPUTEOF DS    0H                                                               
         CLOSE FILEIN                                                           
         CLOSE FILEOUT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
         LTORG                                                                  
* WHEN READING THIS INPUT DATA, CC IS IN COL 0 OF DATA                          
         DS    0D                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VBM,LRECL=200,             X        
               MACRF=GM,EODAD=INPUTEOF                                          
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,LRECL=80,              X        
               MACRF=PM                                                         
         DS    0D                                                               
         DS    F                                                                
REC      DS    2000C                                                            
         EJECT                                                                  
       ++INCLUDE SPNWSCAM                                                       
       ++INCLUDE SPNWSHDR                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPFXNET11/10/96'                                      
         END                                                                    
