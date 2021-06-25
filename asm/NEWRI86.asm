*          DATA SET NEWRI86    AT LEVEL 002 AS OF 01/25/99                      
*PHASE T32086A,*                                                                
*INCLUDE CLPACK                                                                 
         TITLE 'T32086-COLGATE TAPE'                                            
T32086   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NCOL**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
*                                                                               
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    VK                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE REQUEST SCREEN DATA                                 *         
***********************************************************************         
VK       DS    0H                                                               
*                                                                               
         MVC   MYWORK,=C'*MYWORK*' MY WORKING STORAGE DUMP ID                   
*                                                                               
         MVI   NBQINIT,0                                                        
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         MVC   CLTOFFC,COFFICE                                                  
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         XC    CLIENT,CLIENT                                                    
         CLC   =C'ALL',NBSELCLI                                                 
         BE    VK10                                                             
         CLI   NBSELCLI,X'40'                                                   
         BNH   VK10                                                             
         GOTO1 =V(CLPACK),DMCB,NBSELCLI,CLIENT,RR=RELO                          
         DROP  R3                                                               
*                                                                               
*                                                                               
VK10     LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         XC    PRODUCT,PRODUCT                                                  
         CLC   NBSELPRD,=C'POL'                                                 
         BE    VK15                                                             
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    VK15                                                             
*                                                                               
**       MVC   PRODUCT,NBSELPRD                                                 
**       XC    SPLPRO,SPLPRO      SET TO POL SO NETIO PASSES ALL UNITS          
**       MVI   5(R2),3            ELSE PROGRAM MISSES UNITS WHEN BILLED         
**       MVC   SPLPRO(3),=C'POL'  AND THEN PROD ON UNIT CHANGED                 
**       NETGO NVPRDALL,DMCB,0                                                  
*                                                                               
VK15     MVI   FTERMFLG,1              OPTIONAL FIELD                           
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK17                                                             
         NETGO NVESTRNG,DMCB,SPLESTN                                            
                                                                                
* - FUDGE HERE FOR NON-POL REQUESTS                                             
***********************************************************************         
VK17     CLC   NBSELPRD,=C'POL'                                                 
         BE    VK20                                                             
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    VK20                                                             
*                                                                               
         LA    R2,SPLPROH               PRODUCT                                 
         MVC   PRODUCT,NBSELPRD                                                 
         XC    SPLPRO,SPLPRO      SET TO POL SO NETIO PASSES ALL UNITS          
         MVI   5(R2),3            ELSE PROGRAM MISSES UNITS WHEN BILLED         
         MVC   SPLPRO(3),=C'POL'  AND THEN PROD ON UNIT SHANGED                 
         NETGO NVPRDALL,DMCB,0                                                  
         MVC   SPLPRO,PRODUCT                                                   
                                                                                
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK20                                                             
         NETGO NVESTRNG,DMCB,SPLESTN                                            
***********************************************************************         
*                                                                               
VK20     MVI   FTERMFLG,0                                                       
*                                                                               
         LA    R2,SPLSDTH          UNIT START                                   
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLEDTH          UNIT END                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLIVSH          INVOICE DATE START                           
         NETGO NVGETFLD,DMCB                                                    
         GOTO1 DATVAL,DMCB,(0,FLD),DUB                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    DATERR                                                           
         MVC   STRBDAT,DUB       BILL DATE                                      
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)    YMD BINARY                       
         MVC   STARTDAT,FULL       Y/M SERVICE                                  
*                                                                               
         LA    R2,SPLIVEH          INVOICE DATE END                             
         NETGO NVGETFLD,DMCB                                                    
         GOTO1 DATVAL,DMCB,(0,FLD),DUB                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    DATERR                                                           
         MVC   ENDBDAT,DUB         BILL DATE                                    
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)    YMD BINARY                       
         MVC   ENDDAT,FULL         Y/M SERVICE                                  
*                                                                               
         MVI   TAPEOPT,0                                                        
         LA    R2,SPLTAPEH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKEXIT                                                           
         MVC   TAPEOPT,FLD                                                      
         CLI   FLD,C'Y'                                                         
         BNE   VK50                                                             
         CLC   =C'OV',CONWHEN                                                   
         BE   VKEXIT                                                            
         MVI   ERROR,INVALID                                                    
         CLC   =C'ON',CONWHEN                                                   
         BE   VKEXIT                                                            
VK50     CLI   FLD,C'N'                                                         
         BE    VKEXIT                                                           
         B     TRAPERR                                                          
*                                                                               
VKEXIT   LA    R2,SPLCLIH                                                       
         B     EXIT                                                             
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
INVERR   MVI   ERROR,INVALID                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS4                         
*                                                                               
*                                                                               
MYWORK   DS    D                                                                
RELO     DS    F                                                                
ACLISTSV DS    F                                                                
TAPEOPT  DS    CL1                                                              
CLIENT   DS    CL2                                                              
PRODUCT  DS    CL3                                                              
STARTDAT DS    CL2                                                              
ENDDAT   DS    CL2                                                              
STRBDAT  DS    CL6                                                              
ENDBDAT  DS    CL6                                                              
CLTOFFC  DS    CL1                 CLIENT OFFICE                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID3D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
BILHD    DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI86   01/25/99'                                      
         END                                                                    
