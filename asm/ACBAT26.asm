*          DATA SET ACBAT26    AT LEVEL 022 AS OF 05/01/02                      
*PHASE T61B26A                                                                  
         TITLE 'DDS BILLING'                                                    
T61B26   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAN26*,R7,CLEAR=YES                                
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING TWAD,RA                                                          
         SPACE 1                                                                
VALDAT   LA    R2,CMBDATH          VALIDATE THE DATE                            
         MVI   ERRNUM,13                                                        
         BAS   RE,ANY                                                           
         GOTO1 DATVAL,DMCB,(0,CMBDAT),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLC   WORK(2),=C'70'      NOT BEFORE 1970                              
         BL    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDAT)                                    
         SPACE 1                                                                
         MVI   ERRNUM,13           DUE DATE                                     
         LA    R2,CMBDUEDH         FOR CURSOR REPOSITION ON ERROR               
         XC    PDUEDAT,PDUEDAT                                                  
         CLI   CMBDUEDH+5,0                                                     
         BE    CLI                                                              
         GOTO1 DATVAL,DMCB,(0,CMBDUED),PDUEDAT                                  
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR               BAD DATE                                     
         CLC   PDUEDAT(2),=C'78'   DON'T ALLOW PRIOR TO 1978                    
         BL    ERROR                                                            
         SPACE 1                                                                
         LA    R2,CMBBILH                                                       
         BAS   RE,ANY                                                           
         EJECT                                                                  
CLI      LA    R5,COMPEL                                                        
         USING ACCOMPD,R5                                                       
         LA    R2,CMBAGYH          AGENCY                                       
         MVI   ERRNUM,14                                                        
         BAS   RE,ANY                                                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(41),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    U/L FOR PRODUCTION                           
         TM    4(R2),X'20'                                                      
         BO    CLI2                                                             
         FOUT  CMBAGYNH,SPACES,36                                               
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         SPACE 1                                                                
CLI2     XR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),CMBAGY                                                  
         LA    R4,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R4,R3                                                            
         SPACE 1                                                                
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   AGYNUM,KEY                                                       
         MVC   CMBAGYN,ACCTNAME    ACCOUNT NAME                                 
         FOUT  CMBAGYNH                                                         
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
         BAS   RE,PROFMERG         MERGE CLIENT/PROD PROFILES                   
         SPACE 2                                                                
         LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         SPACE 1                                                                
         MVC   SVANAL,SPACES                                                    
         MVC   SVANAL(1),ACPRUNIT                                               
         SPACE 1                                                                
         XR    R6,R6               NO MORE PROFILES                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACPRRECV    RECEIVABLE CODE FROM PROFILE                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         SPACE 1                                                                
ARAC4    MVC   ARNUM,KEY           SAVE A/R ACCOUNT OR ACPRREVC                 
         MVC   ARNAM,ACCTNAME                                                   
         EJECT                                                                  
*              VALIDATE THE INCOME AND COSTING ACCOUNTS                         
         SPACE 1                                                                
VALACC   LA    R2,CMBINCH          SYSTEM/MEDIA                                 
         BAS   RE,ANY                                                           
         MVI   ERRNUM,2                                                         
         OC    CMBINC,SPACES                                                    
VALACC2  DS    0H                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SI'                                                  
         MVC   KEY+3(2),CMBINC                                                  
         LA    R2,CMBINCH                                                       
         LA    R6,WORK             FORCE READ OF INCOME ACCOUNT                 
         BAS   RE,GETACC           GET THE RECORD                               
         SR    R6,R6                                                            
         BAS   RE,CHECKACC         CHECK FOR POSTING                            
         MVC   INCNUM,KEY                                                       
         MVC   INCNAM,ACCTNAME                                                  
         MVC   CMBINCN,ACCTNAME                                                 
         SPACE 1                                                                
         SPACE 1                                                                
         FOUT  CMBINCNH                                                         
         EJECT                                                                  
*              VALIDATE AMOUNT FIELDS                                           
         SPACE 2                                                                
AMNTS    DC    0H'0'                                                            
         LA    R2,CMBGRSH          GRS AMOUNT                                   
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   GRS,DUB                                                          
         SPACE 1                                                                
         LA    R2,CMBDUEH          AMOUNT DUE                                   
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         BAS   R8,VALCASH                                                       
         ZAP   COM,DUB                                                          
         B     POSTING                                                          
         SPACE 1                                                                
VALCASH  MVI   ERRNUM,25           INVALID AMOUNT                               
         GOTO1 AMTVAL,DMCB,(R4),(R3)                                            
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     R4,DMCB+4                                                        
         ZAP   DUB,0(8,R4)                                                      
         BR    R8                                                               
         EJECT                                                                  
POSTING  LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         LA    R2,CMBBILH                                                       
         BAS   RE,ANY                                                           
         MVC   DLDSREF,SPACES                                                   
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),CMBBIL                                                
         SPACE 1                                                                
         MVC   DLDSDATE,PDAT                                                    
         MVI   DLDSSBRF,0                                                       
         XC    DLDSSTAT(7),DLDSSTAT                                             
         SPACE 1                                                                
         LA    R2,CMBNARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SPACE 1                                                                
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               ELEMENT - NARRATIVE                          
         AH    R5,=H'2'                                                         
         AR    R5,R6               R6 = L'NARRATIVE                             
         STH   R5,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         SH    R5,=H'2'                                                         
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         ZIC   R3,DLDSLEN                                                       
         AR    R8,R3                                                            
         SPACE 1                                                                
         USING TRDUED,R8                                                        
         OC    PDUEDAT,PDUEDAT     IF PDUEDAT CREATE A 61                       
         BZ    POSTIC                                                           
         MVC   TRDUEL(2),=X'6104'                                               
         GOTO1 DATCON,DMCB,(0,PDUEDAT),(2,TRDUDATE)                             
         IC    R3,TRDUEN                                                        
         AR    R8,R3                                                            
         SPACE 1                                                                
         USING DLPOSTD,R8                                                       
POSTIC   DS    0H                                                               
         MVC   DLPSEL(2),=X'6971'  DEBIT RECEIVABLES                            
         MVC   DLPSDBAC,ARNUM                                                   
         MVC   DLPSDBNM,ARNAM                                                   
         MVC   DLPSCRAC,SPACES                                                  
         LA    R2,DLPSCRAC+3                                                    
         GOTO1 CHOPPER,DMCB,(36,INCNAM),(12,(R2)),(0,1)                         
         LA    R2,CMBSRCH                                                       
         CLI   5(R2),0             IS ACCOUNT NAME ON SCREEN                    
         BE    POST2               NO - GET OUT                                 
         MVC   DLPSCRAC+3(12),SPACES   YES - USE IT INSTEAD OF                  
         ZIC   R3,CMBSRCH+5            CHOPPED INCNAM                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+3(0),CMBSRC                                             
         SPACE 1                                                                
         SPACE 1                                                                
POST2    MVC   DLPSCRNM,SPACES     SO NAME IS SPACES                            
         MVI   DLPSTYPE,0          SUBSIDIARY                                   
         ZAP   DLPSAMNT,COM                                                     
         MVC   DLPSANAL,SVANAL                                                  
         SPACE 1                                                                
         EJECT                                                                  
POST2A   IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         SPACE 1                                                                
         USING TRCASHD,R8                                                       
POST3    MVC   TRCSEL(2),=X'5009'                                               
         MVI   TRCSTYPE,C'G'       GROSS                                        
         ZAP   TRCSAMNT,GRS        SPECIAL AMOUNT                               
         IC    R3,TRCSLEN                                                       
         AR    R8,R3                                                            
         SPACE 1                                                                
         USING DLPOSTD,R8                                                       
         MVC   DLPSEL(2),=X'6A71'  CREDIT INCOME                                
         MVC   DLPSDBAC,ARNUM      CONTRA IS RELEIVABLE                         
         MVC   DLPSDBNM,ARNAM                                                   
         MVC   DLPSCRAC,INCNUM                                                  
         MVC   DLPSCRNM,INCNAM                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,COM        COMMISSION                                   
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         EJECT                                                                  
RECADD   MVI   0(R8),0             END OF RECORD                                
         LA    R8,1(R8)                                                         
         LA    R3,IOAREA           GRT LENGTH                                   
         SR    R8,R3                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         SPACE 1                                                                
         SR    R3,R3                                                            
         XC    WORK,WORK                                                        
         IC    R3,CMBBILH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CMBBIL                                                   
         ZAP   TRANSAMT,COM                                                     
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)                                                 
         BAS   RE,ADTWA1                                                        
         SPACE 1                                                                
         LA    R2,CMBDATH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATD5D                                                       
         EJECT                                                                  
PROGD    DSECT                                                                  
SET      DS    CL1                                                              
AGYNUM   DS    CL15                                                             
ARNUM    DS    CL15                                                             
ARNAM    DS    CL36                                                             
INCNUM   DS    CL15                                                             
INCNAM   DS    CL36                                                             
SVANAL   DS    CL2                                                              
COM      DS    D                   COMMISSION                                   
GRS      DS    D                                                                
PDAT     DS    CL3                 PACKED DATE                                  
PDUEDAT  DS    CL6                                                              
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         SPACE 2                                                                
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACBAT26   05/01/02'                                      
         END                                                                    
