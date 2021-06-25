*          DATA SET NEMED4C    AT LEVEL 042 AS OF 05/01/02                      
*PHASE T31E4CA,+0                                                               
         TITLE '-  EDIT FOR NETWORK SCHEDULE'                                   
T31E4C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SCED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          PASS ARGS TO PRINT IS W/S AREA 2             
         USING NDDEMBLK,R7                                                      
         ST    R7,NBADEM           DEMO BLOCKS ARE PASSED HERE TOO              
         LA    R1,STALIST                                                       
         ST    R1,NBCNVNTI                                                      
         EJECT                                                                  
*        INITIALIZE                                                             
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          CLIENT RECORD PASSED IN W/S AREA 1           
         ST    R2,NBACLI                                                        
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
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
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK   ALLOWS RANGE OF ESTS            
         OI    SPLESTNH+6,X'80'                   AND ALL,NO                    
         NETGO NVDELHOM,DMCB,NDDEMOS     REMOVE HOMES FROM DEMOLIST             
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN   DAYPART. OPTIONAL.                          
         OI    SPLDPTNH+6,X'80'    TRANSMIT DAYPART NAME                        
*                                                                               
         LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN  PACKAGE. OPTIONAL. ALLOW                  
*                                        LOCK, BOTH                             
         OI    SPLPAKNH+6,X'80'    TRANSMIT PACKAGE NAME                        
*                                                                               
*                                                                               
         LA    R2,SPLFLAVH                                                      
         LA    R3,FLAVLIST                                                      
         BAS   RE,LISTVAL                                                       
         MVC   FLAVOR,SPLFLAV                                                   
*                                                                               
         LA    R2,SPLDOPTH                                                      
         LA    R3,DOPTLIST                                                      
         BAS   RE,LISTVAL                                                       
         MVC   DAYOPT,SPLDOPT                                                   
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
ED3B     LA    R2,SPLDEMH                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED3C                                                             
         NETGO NVDELHOM,DMCB,NDDEMOS     REMOVE HOMES FROM DEMOLIST             
*                                                                               
ED3C     LA    R2,SPLFILTH                                                      
         NETGO NVFILT,DMCB                                                      
*                                                                               
         LA    R2,SPLFORMH                                                      
         NETGO NVGETFLD,DMCB       FORMAT. OPTIONAL.                            
         BZ    ED4                                                              
         LTR   R0,R0               R0 IS 0 IF NON-NUMERIC.                      
         BZ    EDINVERR                                                         
         CH    R0,=H'12'                                                        
         BH    EDINVERR                                                         
         STC   R0,FORMAT                                                        
*                                                                               
ED4      LA    R2,SPLPCDH                                                       
         MVC   PRGCDFLG,SPLPCD     SET TO Y TO USE PROG CODES                   
*                                                                               
         LA    R2,SPLPFBH          TEST IF PRINTING BONUS                       
         CLI   8(R2),C'Y'                                                       
         BNE   ED5                                                              
         MVI   PFBFLG,C'Y'                                                      
*                                                                               
ED5      LA    R2,SPLPROGH         PROG FILTER                                  
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED6                                                              
         MVC   NBSELPRG,FLD                                                     
*                                                                               
ED6      OI    NBINDS,X'80'        DEFAULT TO EQUIVALENCE OVERRIDES             
         LA    R2,SPLOPTH          OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    ED8                                                              
         GOTO1 SCANNER,DMCB,(R2),(2,WORK),0                                     
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ED8                                                              
         LA    R3,WORK                                                          
ED6AA    CLC   12(5,R3),=C'HUNDRED'                                             
         BNE   ED6A                                                             
         MVI   HUNOPT,C'Y'                                                      
         B     EDNEXT                                                           
*                                                                               
ED6A     CLC   12(3,R3),=C'PRE'    CABLE PRECISION                              
         BNE   ED6B                                                             
         CLC   22(3,R3),=C'CAB'                                                 
         BNE   ED6B                                                             
         MVI   NBPREOPT,C'Y'                                                    
         MVI   NBHUNOPT,C'Y'                                                    
         B     EDNEXT                                                           
*                                                                               
ED6B     CLC   12(3,R3),=C'INTG'   INTEGRATION (+ ACTUAL)                       
         BNE   ED6C                                                             
         MVI   NBUSER+15,C'Y'      SET PROFILE                                  
         B     EDNEXT                                                           
*                                                                               
ED6C     CLC   12(3,R3),=C'EQU'    EQUIVALENCE OVERRIDEAS                       
         BNE   ED6D                                                             
         CLI   22(R3),C'N'                                                      
         BNE   ED6D                                                             
         NI    NBINDS,X'FF'-X'80'  DONT EQUIV OVERRIDES                         
         B     EDNEXT                                                           
*                                                                               
ED6D     CLC   =C'PRIMP',12(R3)      INCREASED PRECISION                        
         BNE   EDINV                                                            
         OI    NBINDS,X'40'                                                     
         B     EDNEXT                                                           
*                                                                               
EDNEXT   LA    R3,32(R3)                                                        
         BCT   R5,ED6AA                                                         
         B     ED8                                                              
*                                                                               
EDINV    B     EDINVERR                                                         
*                                                                               
ED8      LA    R2,SPLUTOH          UNIT TOTALS                                  
         CLI   5(R2),0                                                          
         BE    EDXIT                                                            
         MVC   UTOTOPT(1),8(R2)                                                 
*                                                                               
EDXIT    LA    R2,SPLCLIH                                                       
         SPACE 2                                                                
         B     XMOD                                                             
*                                                                               
EDINVERR MVI   ERROR,INVALID                                                    
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
LISTVAL  NTR1                                                                   
         SPACE 2                                                                
LISTVAL2 CLC   0(1,R3),8(R2)                                                    
         BE    XIT                                                              
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   LISTVAL2                                                         
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 2                                                                
FLAVLIST DC    C'EPV',X'FF'                                                     
DOPTLIST DC    C'MQW',X'FF'                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NETUNIVD                                                       
*                                                                               
         EJECT                                                                  
*** ARGS TO  PRINT                                                              
*                                                                               
FLAVOR   DS    CL1                                                              
DAYOPT   DS    CL1                                                              
FORMAT   DS    CL1                                                              
PRGCDFLG DS    CL1                 Y IF USE PROG CODE NOT PROG NAME             
PFBFLG   DS    CL1                 Y=PRINT PFB                                  
HUNOPT   DS    CL1                 Y=PROCESS IN HUNDREDS                        
UTOTOPT  DS    CL1                 Y=SHOW UNIT TOTALS                           
         DS    CL7                 SPARE                                        
*                                                                               
*                                                                               
*** LOCAL W/S (IN PRINT MODULE)                                                 
*                                                                               
PERTYPE  DS    CL3                 1ST BYTE IS PERIOD TYPE                      
MAXMONTS EQU   53                  MAX MONS (WKS) IN LIST                       
MONLIST  DS    CL(4*MAXMONTS)                                                   
NUMMONS  DS    F                                                                
ESTFLAG  DS    F                   SET WHEN GET 1ST ESTIMATE DEMOS              
MONUNIT  DS    H                                                                
TOTUNIT  DS    H                                                                
*                                                                               
         DS    0D                                                               
SPOTAC   DS    CL32                                                             
PERAC    DS    CL32                                                             
SCHEDAC  DS    CL32                                                             
EPLOPT   DS    CL1                                                              
MYDUB    DS    D                                                                
MENU     DS    CL1                                                              
WORKDEMS DS    CL24                                                             
ZEROASS  DS    CL1                                                              
ZEROACT  DS    CL1                                                              
ZEROTHIS DS    CL1                                                              
*                                                                               
STALIST  DS    CL2000                                                           
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDECD                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042NEMED4C   05/01/02'                                      
         END                                                                    
