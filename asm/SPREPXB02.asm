*          DATA SET SPREPXB02  AT LEVEL 023 AS OF 05/01/02                      
*PHASE SPXB02A                                                                  
         TITLE 'SPREPXB02 - BRAND AGENCY INTERFACE'                             
         PRINT NOGEN                                                            
SPXB02   CSECT                                                                  
         DS    4000C                                                            
         ORG   *-4000                                                           
         NMOD1 0,SPXB02                                                         
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING XBWORKD,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        RUN FIRST                                                              
*                                                                               
RUNF     DS    0H                                                               
         CLI   INITSW,0                                                         
         BNE   INITX                                                            
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
         MVC   P+2(14),=C'RECORDS OUTPUT'                                       
         EDIT  OUTCNT,(12,P+18),0,COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 REPORT                                                           
*                                                                               
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
CLTF     L     R8,ADCLT            A(CLIENT RECORD)                             
*                                                                               
CLT10    GOTO1 SEQ                 READ TIL REEACH EST                          
         CLC   0(1,R8),KEY         SAME AGY/MED(/CLT)                           
         BNE   CLTX                                                             
         OC    KEY+7(1),KEY+7      IS THIS AN ESTIMATE                          
         BZ    CLT10                                                            
         OC    KEY+8(5),KEY+8                                                   
         BNZ   CLT10                                                            
         GOTO1 GETEST              YES - GET THE ESTIMATE                       
         L     R6,ADEST                                                         
         USING ESTRECD,R6                                                       
         CLC   EUSER2,SPACES       IF THERE IS SOMETHING IN EUSER2 FLD          
         BNH   CLT10                                                            
         CLC   QSTART,C' '         IS THERE A DATE TO FILTER ON                 
         BE    CLT20               NO -                                         
         CLC   QSTART,EEND         REQ START AFTER EST END                      
         BH    CLT10                                                            
         CLC   QEND,ESTART         REQ END BEFORE EST START                     
         BL    CLT10                                                            
*                                                                               
CLT20    XC    OUTREC,OUTREC                                                    
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         MVI   OSYSTEM,C'S'        SET SYSTEM                                   
         CLI   MCNETPAK,C'Y'                                                    
         BNE   *+8                                                              
         MVI   OSYSTEM,C'N'                                                     
         MVC   OMEDIA,QMED         EXTRACT DATA                                 
         GOTO1 CLUNPK,DMCB,EKEYCLT,OCLT                                         
         MVC   OPRD,EKEYPRD                                                     
         EDIT  EKEYEST,(3,OEST),FILL=0                                          
         MVC   OAGYCD,EUSER2                                                    
         OC    OAGYCD,SPACES                                                    
         BAS   RE,PUTFILE          PUT TO FILE                                  
         B     CLT10                                                            
*                                                                               
CLTX     GOTO1 AENDREQ             NEXT REQUEST                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        PUT RECORD TO DATA SET                                                 
*                                                                               
PUTFILE  NTR1                                                                   
         LA    R0,OUTREC                                                        
         LA    R1,OUTFILE                                                       
         PUT   (1),(0)                                                          
*                                                                               
         AP    OUTCNT,=P'1'        INCREMENT N'OUTPUT TO FILE                   
         LA    R2,P                                                             
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
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=FB,LRECL=16,              X        
               BLKSIZE=8000,MACRF=PM                                            
*                                                                               
OUTLEN   EQU   500                                                              
INITSW   DC    X'00'                                                            
FFS      DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
         EJECT                                                                  
XBWORKD  DSECT                                                                  
*                                                                               
OUTCNT   DS    PL6                                                              
*                                                                               
OUTREC   DS    0CL16                                                            
OSYSTEM  DS    CL1                 SYSTEM                                       
OMEDIA   DS    CL1                 MEDIA                                        
OCLT     DS    CL3                 CLIENT                                       
OPRD     DS    CL3                 PRODUCT                                      
OEST     DS    CL3                 ESTIMATE                                     
OAGYCD   DS    CL3                 AGENCY CODE - 1ST 3 BYTES OF EUSER2          
         DS    CL2                 SPARE                                        
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
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPREPXB02 05/01/02'                                      
         END                                                                    
