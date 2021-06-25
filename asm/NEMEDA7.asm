*          DATA SET NEMEDA7    AT LEVEL 033 AS OF 05/01/02                      
*PHASE T31EA7A                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T31EA7 - Y R REPORT  PHASE'                                     
T31EA7   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**YR02**,RR=R2                                                 
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYD,R7                                                           
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALKEY                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
************************************************************                    
*              EDIT MODULE                                                      
********************************************************                        
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'B'         WILL WANT PACKAGE RECORDS FIRST              
*                                                                               
         LA    R1,NDDEMBLK                                                      
         ST    R1,NBADEM           FOR DEMOS                                    
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH          CLIENT. REQUIRED.                            
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'    TRANSMIT PRODUCT NAME.                       
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         LA    R2,SPLESTH          ESTIMATE. OPTIONAL                           
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         OI    SPLESTNH+6,X'80'    XMIT ESTIMATE NAME                           
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLPAKH                                                       
         NETGO NVPAK,DMCB,SPLPAKN  PACKAGE. OPTIONAL. ALLOW                     
         OI    SPLPAKNH+6,X'80'    TRANSMIT PACKAGE NAME                        
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN   DAYPART. OPTIONAL.                          
         OI    SPLDPTNH+6,X'80'    TRANSMIT DAYPART NAME                        
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         LA    R2,SPLSKEDH                                                      
         NETGO NVGETFLD,DMCB                                                    
         MVC   NBSELUOP,FLD        SET SELUOP TO 'E' OR 'A'                     
         CLI   FLD,C'A'                                                         
         BE    ED8                                                              
         CLI   FLD,C'E'                                                         
         BE    ED8                                                              
         B     EDINV                                                            
*                                                                               
ED8      LA    R2,SPLDEMFH                                                      
         NETGO NVGETFLD,DMCB                                                    
         CLI   FLD,C'A'            ACTUAL DEMOS                                 
         BNE   ED10                                                             
         MVI   NBACTOPT,C'Y'                                                    
         B     ED12                                                             
ED10     CLI   FLD,C'E'                                                         
         BNE   EDINV                                                            
         MVI   NBESTOPT,X'04'      RETURN EST DEMOS FOR MGS ALSO                
         MVI   NBESTFCT,6          (REJECT IF STATUS=04 (PREEMPT))              
*                                                                               
ED12     MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         LA    R2,SPLDEMH                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
         MVC   TITLE,=CL40'PROGRAM AVERAGE REPORT'     DEFAULT TITLE            
         LA    R2,SPLTITH          TITLE                                        
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED14                                                             
         MVC   TITLE(40),FLD                                                    
*                                                                               
ED14     LA    R2,SPLOPTH          OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    ED20                                                             
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
*                                                                               
ED16     CLC   12(4,R3),=C'DOWN'   OPTION TO DOWNLOAD                           
         BNE   *+12                                                             
         MVI   DOWNOPT,C'Y'                                                     
         B     ED18                                                             
         CLC   12(4,R3),=C'RANK'   RANK CPP REPORT                              
         BNE   *+12                                                             
         MVI   DPGOPT,C'A'                                                      
         B     ED18                                                             
         B     EDINV                                                            
*                                                                               
ED18     LA    R3,32(R3)                                                        
         BCT   R0,ED16                                                          
*                                                                               
ED20     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
EDERR    GOTO1 ERREX,DMCB                                                       
         EJECT                                                                  
*******************************************************                         
*              REPORT MODULE                                                    
*******************************************************                         
REPMOD   NTR1                                                                   
*              INITIALIZE FOR DRIVER                                            
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9031EB7'  LOAD T31EB7 (GLOBAL STORAGE)          
         L     R6,DMCB                                                          
         USING GLOBALD,R6                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'  LOAD T00A3A (DRIVER)                  
         MVC   DRIVER,DMCB                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A41'  LOAD T00A41 (NETWORK DRIVER)          
         MVC   GLASYSDR,DMCB                                                    
         GOTO1 CALLOV,DMCB,0,X'D9031E97'  LOAD T31E97 (DPG PHASE)               
         MVC   GLAPROG,DMCB                                                     
*                                                                               
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
*                                                                               
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
*                                                                               
         LA    R1,0                GET NUM DEMOS IN R1                          
         LA    R2,NDDEMOS                                                       
         LA    R3,20               MAX 20 DEMOS                                 
YR2      OC    0(3,R2),0(R2)                                                    
         BZ    YR4                                                              
         LA    R1,1(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   R3,YR2                                                           
YR4      STC   R1,GLOPTS           PASS IT IN GLOPTS                            
         MVI   GLSPACE,2                                                        
*                                                                               
         CLI   DOWNOPT,C'Y'        OPTION TO DOWNLOAD                           
         BNE   *+8                                                              
         MVI   GLDOWNLD,X'80'                                                   
         MVC   GLOPTS+1(1),DPGOPT  PASS REPORT TYPE AS OPT 2                    
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R6)                                                 
         EJECT                                                                  
*              INPUT - PROGRAM I/O                                              
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    GOTONE                                                           
         CLI   NBMODE,NBREQLST                                                  
         BE    ALLDONE                                                          
         CLI   NBERROR,0                                                        
         BE    GETUNIT                                                          
         DC    H'0'                                                             
*                                                                               
GOTONE   OC    NBPROGNM,=16XL1'40'    REPLACE BINARY ZEROES W/ SPACES           
*                                     FOR SORTING BY PROG NAME                  
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     GETUNIT                                                          
*                                                                               
*  ALL DONE WITH INPUT                                                          
ALLDONE  MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,49,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
*                                                                               
HOOK     NTR1                              HEAD HOOK                            
*                                                                               
         CLI   GLHOOK,GLHEAD                                                    
         BNE   HKXIT                                                            
*                                                                               
         MVC   H1+49(40),TITLE                                                  
         GOTO1 UNDERLIN,DMCB,(40,H1+49),H2+49                                   
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+18(20),SPLCLIN                                                
         MVC   H5+18(20),SPLPRON                                                
         MVC   H6+18(24),SPLESTN                                                
         MVC   H5+90(8),SPLDPTN                                                 
         CLI   NBSELPAK,0                                                       
         BE    *+10                                                             
         MVC   H6+74(36),SPLPAKN                                                
*                                                                               
HKXIT    B     XIT                                                              
*                                                                               
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              STORAGE  LTORG ETC                                               
         SPACE 3                                                                
RELO     DS    A                                                                
DRIVER   DS    A                                                                
         SPACE 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
*              NETINCLS AND MODULE W/S                                          
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 1                                                                
MYD      DSECT                                                                  
******* W/S                                                                     
TITLE    DS    CL40                                                             
DPGOPT   DS    CL1                                                              
DOWNOPT  DS    CL1                                                              
*                                                                               
*                                  DEDBLOCK & NETDEMOD                          
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NETDEMOD                                                       
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD7D                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033NEMEDA7   05/01/02'                                      
         END                                                                    
