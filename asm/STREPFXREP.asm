*          DATA SET STREPFXREP AT LEVEL 010 AS OF 08/28/95                      
*PHASE SPFX02U                                                                  
*                                                                               
         TITLE 'STREPFXREP  DELETE ALPHA REP FOR ZENY'                          
***********************************************************************         
*                                                                               
SPFX02   CSECT                                                                  
*        PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8                                                      
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQF                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
RQF      XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         MVI   KEY,C'R'            REP RECORDS ONLY                             
*                                                                               
RQF5     GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     RQF20                                                            
*                                                                               
RQF10    GOTO1 SEQSTA                                                           
*                                                                               
RQF20    DS    0H                                                               
         L     R5,ADSTAT           ADSTAT-->RECORD FOUND.                       
         L     R1,READCNT                                                       
         LA    R1,1(R1)            INCREMENT RECORDS READ.                      
         ST    R1,READCNT                                                       
                                                                                
         CLI   0(R5),C'R'          IS IT A REP RECORD?                          
         BNE   RQF100                                                           
                                                                                
         CLC   =C'TH',5(R5)        AGENCY TH (ZENY)                             
         BNE   RQF10                                                            
                                                                                
         LA    R1,2(R5)            CHECK IF NUMERIC REP CODE                    
         LA    R2,3                                                             
RQF30    CLI   0(R1),C'1'                                                       
         BL    RQF40                                                            
         CLI   0(R1),C'9'                                                       
         BH    RQF40                                                            
         LA    R1,1(R1)                                                         
         BCT   R2,RQF30                                                         
                                                                                
         B     RQF10               YES IT IS NUMERIC                            
                                                                                
RQF40    OI    17(R5),X'80'                                                     
         MVC   SAVEKEY,0(R5)                                                    
         CLI   RCWRITE,C'Y'                                                     
         BNE   RQF50                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,(R5)                  
                                                                                
RQF50    MVC   P(7),SAVEKEY            PRINT RECORD                             
         GOTO1 HEXOUT,DMCB,(R5),P+10,18,=C'TOG'                                 
         GOTO1 REPORT                                                           
         L     R1,INVLCNT                                                       
         LA    R1,1(R1)            INCREMENT # OF TYPE-'A'S.                    
         ST    R1,INVLCNT                                                       
                                                                                
         B     RQF10                                                            
                                                                                
********************  PROCESSING END, SHOW QUOTAS  ********************         
RQF100   MVC   P1(26),=C'NUMBER OF RECORDS READ  = '                            
         EDIT  READCNT,(10,P1+28),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         MVC   P1(26),=C'NUMBER OF INVALID  READ = '                            
         EDIT  INVLCNT,(10,P1+28),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
**************************** MISCELLANEOUS ****************************         
INVLCNT  DS    F                   # OF INVALID S RECORDS                       
READCNT  DS    F                   # OF RECORDS READ.                           
*                                                                               
MYWORK   DS    CL17                                                             
SAVEKEY  DS    XL15                                                             
         DS    0F                                                               
STAWORK  DS    XL31                STAPACK DSECT                                
         EJECT                                                                  
************************* FIXED-RECORDS DSECT *************************         
       ++INCLUDE SPGENMSTA                                                      
         EJECT                                                                  
       ++INCLUDE SPGENANMK                                                      
         EJECT                                                                  
************************** REP-RECORDS DSECT **************************         
*                                                                               
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREPT                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010STREPFXREP08/28/95'                                      
         END                                                                    
