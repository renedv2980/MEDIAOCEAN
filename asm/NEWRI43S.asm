*          DATA SET NEWRI43S   AT LEVEL 025 AS OF 05/01/02                      
*PHASE T32043A                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE CASHVAL                                                                
         TITLE 'T32043 - PNG REPORT EDIT PHASE'                                 
T32043   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NW43**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3         ANETWS3=WORKING STORAGE                       
         USING MYD,R7                                                           
         MVC   NBACLI,ANETWS1     CLIENT RECORD IN WS1                          
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,EDITMOD                                                       
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB,SPLNETN                                            
         OI    SPLNETNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPT,DMCB,SPLDAYN                                               
         OI    SPLDAYNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPAKH                PACKAGE                                
         NETGO NVPAKLOK,DMCB                                                    
*                                                                               
         LA    R2,SPLSDTH                START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLEDTH                END DATE                               
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLCALH                                                       
         CLI   5(R2),0                                                          
         BNE   EDT8                                                             
         MVI   SPLCAL,C'C'         DEFAULT IS CALENDAR                          
         OI    SPLCALH+6,X'80'                                                  
         B     EDT10                                                            
EDT8     CLI   8(R2),C'B'                                                       
         BE    EDT10                                                            
         CLI   8(R2),C'C'                                                       
         BNE   EDINV                                                            
*                                                                               
EDT10    LA    R2,SPLDWNH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT11                                                            
         MVC   DOWNOPT,FLD                                                      
         CLI   FLD,C'Y'                                                         
         BNE   EDT12                                                            
EDT11    MVI   SPLDWN,C'Y'                                                      
         OI    SPLDWNH+6,X'80'                                                  
         MVC   CONOUT(4),=C'DOWN'                                               
         OI    CONOUTH+6,X'80'                                                  
         B     EDT22                                                            
EDT12    CLI   FLD,C'N'                                                         
         BNE   EDINV                                                            
         SPACE 1                                                                
EDT22    DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
EDERR    GOTO1 ERREX                                                            
         EJECT                                                                  
*        LTORG                                                                  
*                                                                               
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
DOWNOPT  DS    CL1                                                              
RELO     DS    A                                                                
*                                                                               
*                                                                               
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID1D                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NEWRI43S  05/01/02'                                      
         END                                                                    
