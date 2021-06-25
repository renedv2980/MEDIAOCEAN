*          DATA SET PPREPXB02  AT LEVEL 032 AS OF 05/01/02                      
*PHASE PPXB02C                 ***** NOTE "C" PHASE                             
         TITLE 'PPREPXB02 - PRINTPAK BRAND INTERFACE'                           
PPXB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPXB02,R9                                                      
         L     RC,0(R1)                                                         
         USING PPWORKD,RC         GLOBAL WORKING STORAGE                        
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         MVI   RC2DSECT,C'Y'      USE SECOND DSECT                              
         L     R8,PPWORK2C                                                      
         USING PPWORK2D,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,PROCCLI                                                     
         BE    CLTF                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        RUN FIRST                                                              
*                                                                               
RUNF     DS    0H                                                               
         CLI   INITSW,0                                                         
         BNE   INITX                                                            
         LA    R6,IO                                                            
         ST    R6,AREC                                                          
         ZAP   OUTCNT,=P'0'                                                     
         LA    R2,OUTFILE                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   INITSW,1                                                         
*                                                                               
INITX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        RUN LAST                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P1+2(14),=C'RECORDS OUTPUT'                                      
         EDIT  OUTCNT,(12,P1+18),0,COMMAS=YES,ZERO=NOBLANK                      
         GOTO1 REPORT                                                           
         MVC   OUTREC,FFS          END OF FILE ALL 'FF'                         
         LA    R0,OUTREC                                                        
         LA    R1,OUTFILE                                                       
         PUT   (1),(0)                                                          
         CLOSE (OUTFILE)                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        CLIENT FIRST                                                           
*                                                                               
CLTF     MVC   MYKEY,KEY           CLIENT KEY                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVC   0(3,R2),MYKEY       SET AGY/MED/ /CLT                            
         MVI   3(R2),X'07'         OVERRIDE RECORD CODE                         
         GOTO1 HIGH                GET EST                                      
         B     CLT20                                                            
*                                                                               
CLT10    GOTO1 SEQ                 READ ALL ESTIMATES                           
*                                                                               
CLT20    CLC   KEY(4),KEYSAVE      SAME AGY/MED & STILL ESTIMATE                
         BNE   CLTX                                                             
         GOTO1 GETPRT              YES - GET THE ESTIMATE                       
         L     R6,AREC                                                          
         USING PESTREC,R6                                                       
         MVI   ELCODE,X'08'                                                     
         LA    R2,PESTELEM                                                      
         USING PESTUDEF,R2                                                      
         BAS   RE,NEXTEL                                                        
         BNE   CLT10                                                            
         CLC   PEUSER2,SPACES      IF THERE IS SOMETHING IN EUSER2 FLD          
         BNH   CLT10                                                            
         CLI   QSTART,C' '         ANY DATE FILTERING?                          
         BE    CLT25                                                            
         CLC   QSTART,PESTEND                                                   
         BH    CLT10                                                            
         CLC   QEND,PESTST                                                      
         BL    CLT10                                                            
*                                                                               
CLT25    XC    OUTREC,OUTREC                                                    
         MVI   OSYSTEM,C'P'                                                     
         MVC   OAGYCD,PEUSER2                                                   
         OC    OAGYCD,SPACES                                                    
         MVC   OMEDIA,PESTKMED     EXTRACT DATA                                 
         MVC   OCLT,PESTKCLT                                                    
         MVC   OPRD,PESTKPRD                                                    
         EDIT  PESTKEST,(3,OEST),FILL=0                                         
         BAS   RE,PUTFILE          PUT TO FILE                                  
         B     CLT10                                                            
*                                                                               
CLTX     GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
*                                                                               
*        PUT RECORD TO DATA SET                                                 
*                                                                               
PUTFILE  NTR1                                                                   
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
         AP    OUTCNT,=P'1'        INCREMENT N'OUTPUT TO FILE                   
         LA    R2,P1                                                            
         USING LISTD,R2                                                         
         MVC   LSYSTEM,OSYSTEM     PRINT REPORT                                 
         MVC   LMEDIA,OMEDIA                                                    
         MVC   LCLT,OCLT                                                        
         MVC   LPRD,OPRD                                                        
         MVC   LEST,OEST                                                        
         MVC   LAGYCD,OAGYCD                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTEL2  DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=FB,LRECL=16,              X        
               BLKSIZE=8000,MACRF=PM                                            
*                                                                               
INITSW   DC    X'00'                                                            
FFS      DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
*                                                                               
OUTCNT   DS    PL6                                                              
MYKEY    DS    CL(L'KEY)                                                        
*                                                                               
OUTREC   DS    0CL16                                                            
OSYSTEM  DS    CL1                 SYSTEM                                       
OMEDIA   DS    CL1                 MEDIA                                        
OCLT     DS    CL3                 CLIENT                                       
OPRD     DS    CL3                 PRODUCT                                      
OEST     DS    CL3                 ESTIMATRE                                    
OAGYCD   DS    CL3                 SGENCY CODE - 1ST 3 BYTES OF EUSER2          
         DS    CL2                 SPARE                                        
*                                                                               
IO       DS    2000C                                                            
*                                                                               
LISTD    DSECT                     DSECT FOR PRINT LINE                         
         DS    CL2                                                              
LSYSTEM  DS    CL1                 SYSTEM                                       
         DS    CL4                                                              
LMEDIA   DS    CL1                 MEDIA                                        
         DS    CL3                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL2                                                              
LPRD     DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
LEST     DS    CL3                 ESTIMATE                                     
         DS    CL4                                                              
LAGYCD   DS    CL3                 AGENCY CODE                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032PPREPXB02 05/01/02'                                      
         END                                                                    
