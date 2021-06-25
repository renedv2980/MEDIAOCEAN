*          DATA SET SREML00    AT LEVEL 001 AS OF 08/25/04                      
*PHASE T17700A                                                                  
         TITLE '$EMAIL - FORMAT EMAIL FOR JESMAIL'                              
EMAIL    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**$EML**,CLEAR=YES                                         
         USING WORKD,RC             RC=A(W/S)                                   
         USING SRPARMD,R1          R1=A(PARMS)                                  
         L     RA,SRQATWA                                                       
         USING SREMLFFD,RA         RA=A(TWA)                                    
         L     R9,SRQASYSF                                                      
         USING SYSFACD,R9          R9=A(SYSFACS)                                
*                                                                               
         BRAS  RE,INIT             INITIALISE ALL VALUES                        
*                                                                               
         BRAS  RE,VAL              VALIDATE SCREEN DETAILS                      
         BNE   EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL VALUES                                               *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     R2,SRQACOMF                                                      
         USING COMFACSD,R2         R2=A(COMFACS)                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   AJESMAIL,CJESMAIL                                                
INITX    J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FIELDS                                               *         
***********************************************************************         
         SPACE 1                                                                
VAL      NTR1  ,                                                                
         MVI   TOFLDX,X'FF'                                                     
         MVI   FROMFLDX,X'FF'                                                   
         MVI   CCFLDX,X'FF'                                                     
         MVI   BCCFLDX,X'FF'                                                    
         MVI   DATAFLDX,X'FF'                                                   
*                                                                               
         LA    R3,PLIST                                                         
         USING SMTPD,R3                                                         
*                                                                               
         LA    R2,SRVTOH                                                        
         USING FHD,R2                                                           
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BZ    EXITL               REQUIRED                                     
         BCTR  RF,0                                                             
         MVC   TOFLD,SPACES                                                     
         EX    RF,*+4                                                           
         MVC   TOFLD(0),FHDA       SET 'TO'                                     
*                                                                               
         LA    RF,TOFLD                                                         
         ST    RF,SMTPTO           SET IN PARAMTER LIST                         
*                                                                               
         LA    R2,SRVFROMH         FROM FIELD IS OPTIONAL                       
         USING FHD,R2                                                           
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BZ    VAL00                                                            
         BCTR  RF,0                                                             
         MVC   FROMFLD,SPACES                                                   
         EX    RF,*+4                                                           
         MVC   FROMFLD(0),FHDA     SET 'FROM'                                   
*                                                                               
         LA    RF,FROMFLD                                                       
         ST    RF,SMTPFROM         SET IN PARAMTER LIST                         
*                                                                               
VAL00    LA    R2,SRVCCH           CC FIELD IS OPTIONAL                         
         USING FHD,R2                                                           
         XR    RF,RF                                                            
         ICM   RF,1,FHIL           ANY INPUT?                                   
         BZ    VAL02                                                            
         BCTR  RF,0                                                             
         MVC   CCFLD,SPACES                                                     
         EX    RF,*+4                                                           
         MVC   CCFLD(0),FHDA       SET 'CC'                                     
*                                                                               
         LA    RF,CCFLD                                                         
         ST    RF,SMTPCC           SET IN PARAMETER LIST                        
*                                                                               
VAL02    LA    R2,SRVBCCH          BCC FIELD IS OPTIONAL                        
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BZ    VAL04                                                            
         BCTR  RF,0                                                             
         MVC   BCCFLD,SPACES                                                    
         EX    RF,*+4                                                           
         MVC   BCCFLD(0),FHDA      SET 'BCC'                                    
*                                                                               
         LA    RF,BCCFLD                                                        
         ST    RF,SMTPBCC          SET IN PARAMETER LIST                        
*                                                                               
VAL04    LA    R2,SRVSUBH          SUBJECT IS OPTIONAL                          
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BZ    VAL06                                                            
         BCTR  RF,0                                                             
         MVC   SUBFLD,SPACES                                                    
         EX    RF,*+4                                                           
         MVC   SUBFLD(0),FHDA      SET 'SUBJECT'                                
*                                                                               
         LA    RF,SUBFLD                                                        
         ST    RF,SMTPSUB          SET IN PARAMTER LIST                         
*                                                                               
VAL06    LA    R2,SRVDTA1H         DATA COMES AS 80 BYTE FIELDS                 
         LA    R4,DATAFLD          OURS ARE ABOUT 75                            
         MVC   0(80,R4),SPACES                                                  
*                                                                               
VAL08    CLI   FHLN,0                                                           
         BE    VAL12                                                            
*                                                                               
         XR    RF,RF               ONLY FIELDS WITH INPUT                       
         ICM   RF,1,FHIL                                                        
         BZ    VAL10                                                            
*                                                                               
         BCTR  RF,0                                                             
         MVC   0(80,R4),SPACES                                                  
         EX    RF,*+4                                                           
         MVC   0(0,R4),FHDA        SET DATA                                     
         AHI   R4,80                                                            
*                                                                               
VAL10    XR    RF,RF               KEEP GOING DOWN SCREEN                       
         ICM   RF,1,FHLN                                                        
         BXH   R2,RF,VAL08                                                      
*                                                                               
VAL12    MVI   0(R4),X'FF'                                                      
         LA    RF,DATAFLD                                                       
         ST    RF,SMTPDATA         SET A(DATA)                                  
*                                                                               
         GOTO1 AJESMAIL,(R3)                                                    
*                                                                               
VALX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
SPACES   DC    80C' '                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
PLIST    DS    8F                                                               
VGETTXT  DS    V                                                                
AJESMAIL DS    V                                                                
*                                                                               
TOFLD    DS    CL60                                                             
TOFLDX   DS    X                                                                
FROMFLD  DS    CL60                                                             
FROMFLDX DS    X                                                                
CCFLD    DS    CL60                                                             
CCFLDX   DS    X                                                                
BCCFLD   DS    CL60                                                             
BCCFLDX  DS    X                                                                
SUBFLD   DS    CL70                                                             
DATAFLD  DS    20CL80                                                           
DATAFLDX DS    X                                                                
IOAREA   DS    XL2000                                                           
WORKL    EQU   *-WORKD                                                          
*                                                                               
       ++INCLUDE FAJESMAILD                                                     
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SREMLFFD DSECT                                                                  
         DS    CL64                                                             
* SREMLFFD                                                                      
       ++INCLUDE SREMLFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FASRPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SREML00   08/25/04'                                      
         END                                                                    
