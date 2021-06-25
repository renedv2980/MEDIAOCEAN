*          DATA SET SPREPFXNST AT LEVEL 041 AS OF 05/01/02                      
*PHASE SPFX025                                                                  
         TITLE 'SPFX02 - DELETE BAD STATION N PTRS'                             
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
REQF     DS    0H                                                               
*                                                                               
* READ THE AGENCY HEADERS AND BUILD A TABLE                                     
* THAT IDENTIFIES AGENCY AS US OR CANADIAN                                      
*                                                                               
         USING AGYRECD,R6                                                       
         XC    LASTAGY,LASTAGY                                                  
         XC    AGYTAB,AGYTAB                                                    
         LA    R2,AGYTAB                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         GOTO1 HIGH                                                             
         B     BLDAG4                                                           
*                                                                               
BLDAG2   GOTO1 SEQ                                                              
*                                                                               
BLDAG4   CLI   KEY,6                                                            
         BNE   BLDAGX                                                           
         LA    R6,KEY                                                           
         MVC   0(2,R2),AGYKAGY     MOVE AGYALPHA TO TABLE                       
         MVC   2(1,R2),AGYPROF+7   MOVE COUNTRY CODE TO TABLE                   
         LA    R2,3(R2)            NEXT                                         
         LA    R3,AGYTABX                                                       
         CR    R2,R3                                                            
         BL    BLDAG2                                                           
         DC    H'0'                                                             
*                                                                               
BLDAGX   MVI   0(R2),X'FF'         EOT                                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* - REQL - BUILD TABLE OF CLIENT LEVEL 'N' STATION PTRS                         
*                                                                               
REQL     LA    R2,NKEYTAB         TABLE FOR 'N'STATION KEYS                     
         ZAP   TABCNT,=P'0'                                                     
         ZAP   RECCNT,=P'0'                                                     
         MVI   DONE,C'N'                                                        
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         MVI   STNKTYPE,C'N'       STATION RECORD                               
         GOTO1 HIGHSTA                                                          
         B     REQL20                                                           
*                                                                               
REQL04   LA    R2,NKEYTAB          TABLE FOR 'N'STATION KEYS                    
         ZAP   TABCNT,=P'0'                                                     
         MVC   KEY,LASTNPTR        GET NEXT RECORD                              
         MVI   KEY+14,C'1'         FORCE NEXT PTR                               
         GOTO1 HIGHSTA                                                          
         L     R3,ADSTAT           ADSTAT POINTS TO RECORD FOUND                
         B     REQL20                                                           
*                                                                               
REQL10   GOTO1 SEQSTA              GET NEXT RECORD                              
REQL20   L     R3,ADSTAT           ADSTAT POINTS TO RECORD FOUND                
         MVC   LASTNPTR,0(R3)                                                   
         CLI   0(R3),C'N'          STILL N PTRS                                 
         BE    REQL30                                                           
         MVI   DONE,C'Y'           NO MORE N PTRS                               
         B     REQL50                                                           
REQL30   CLC   =C'000',STNKCLT     SKIP IF NO CLT                               
         BE    REQL10                                                           
         MVC   0(15,R2),0(R3)                                                   
         LA    R2,15(R2)           NEXT                                         
         AP    TABCNT,=P'1'                                                     
         CP    TABCNT,=P'15000'                                                 
         BL    REQL10                                                           
*                                                                               
REQL50   MVI   0(R2),X'FF'        EOT                                           
         EJECT                                                                  
*                                                                               
* REQL - READ TABLE AND LOOK FOR MATCHING 'S' STATION REC                       
*                                                                               
         LA    R2,NKEYTAB         TABLE FOR 'N'STATION KEYS                     
REQL60   CLI   0(R2),X'FF'                                                      
         BNE   REQL62                                                           
         CLI   DONE,C'Y'           ANY MORE N PTRS                              
         BE    REQL90                                                           
         B     REQL04              GO GET MORE                                  
*                                                                               
REQL62   CLC   LASTAGY,1(R2)                                                    
         BE    REQL66                                                           
         MVC   LASTAGY,1(R2)                                                    
         BAS   RE,SETAGY           SET AGY AND COUNTRY                          
*                                                                               
REQL66   XC    KEY,KEY             READ FOR STATION RECORD                      
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         MVI   STAKTYPE,C'S'       STATION RECORD                               
         MVC   STAKMED,3(R2)       MEDIA                                        
         MVC   QMED,3(R2)                                                       
         MVC   MKTSTA,4(R2)                                                     
         GOTO1 MSUNPK,DMCB,MKTSTA,MAKT,STATN                                    
         MVC   STAKCALL,STATN                                                   
         CLI   STAKMED,C'T'                                                     
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,1(R2)       AGENCY                                       
         MVC   STAKCLT,9(R2)       CLT                                          
         MVC   STAKFILL,=C'000'                                                 
         GOTO1 HIGHSTA                                                          
         L     R3,ADSTAT           ADSTAT POINTS TO RECORD FOUND                
         CLC   KEY(15),0(R3)                                                    
         BNE   REQL70              MAKE SURE STILL C'S' AND SAME MEDIA          
REQL65   LA    R2,15(R2)   NEXT                                                 
         B     REQL60                                                           
         EJECT                                                                  
*                                                                               
*  DELETE N PTR                                                                 
*                                                                               
REQL70   BAS   RE,PRINTIT          RECORD IS BAD                                
         AP    RECCNT,=P'1'                                                     
*                                                                               
         MVC   KEY(15),0(R2)                                                    
         GOTO1 HIGHSTA                                                          
         L     R3,ADSTAT           ADSTAT POINTS TO RECORD FOUND                
         CLC   KEY(15),0(R3)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    17(R3),X'80'        MARK DELETED                                 
*        BAS   RE,PRINTIT          RECORD IS BAD                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   REQL65                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',ADSTAT,ADSTAT                 
         B     REQL65                                                           
*                                                                               
REQL90   MVC   P(7),=C'ERRORS='                                                 
         EDIT  RECCNT,(12,P+10)                                                 
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - SET AGY AND COUNTRY FROM AGYTAB                                             
*                                                                               
SETAGY   NTR1                                                                   
         MVC   AGY,LASTAGY                                                      
         LA    R4,AGYTAB                                                        
SETAGY10 CLI   0(R4),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   AGY,0(R4)                                                        
         BE    SETAGY20                                                         
         LA    R4,3(R4)                                                         
         B     SETAGY10                                                         
SETAGY20 MVC   COUNTRY,2(R4)       COUNTRY CODE FROM AGYPROF+7                  
         B     EXIT                                                             
*                                                                               
* - PRINT OUT DETAILS OF STATION RECORD TO DELETE                               
*                                                                               
PRINTIT  NTR1                                                                   
         MVC   P(15),0(R2)                                                      
         MVC   P+17(4),=C'STA='                                                 
         MVC   P+21(5),STATN                                                    
         MVC   P+27(4),=C'MKT='                                                 
         MVC   P+31(4),MAKT                                                     
         MVC   P+50(20),KEY                                                     
         MVC   P+70(20),0(R3)                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CONSTANTS                                                                     
*                                                                               
         LTORG                                                                  
DONE     DS    CL1                                                              
LASTAGY  DS    CL2                                                              
LASTNPTR DS    CL15                                                             
AGYTAB   DS    50XL3                                                            
AGYTABX  DS    XL1                                                              
TABCNT   DS    PL6                                                              
RECCNT   DS    PL6                                                              
MKTSTA   DS    XL5                                                              
MAKT     DS    CL4                                                              
STATN    DS    CL5                                                              
NKEYTAB  DS    15000CL15                                                        
         DS    XL1                                                              
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SPREPFXNST05/01/02'                                      
         END                                                                    
