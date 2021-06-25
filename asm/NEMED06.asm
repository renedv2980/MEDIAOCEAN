*          DATA SET NEMED06    AT LEVEL 025 AS OF 05/01/02                      
*PHASE T31E06A                                                                  
         TITLE 'T31E06 - EDIT FOR OVERNIGHT'                                    
T31E06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**OVED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2 FOR ARGS TO PRINT             
         USING EDD,R7                                                           
         ST    R7,NBADEM           1ST PART IS NET DEMO BLOCK                   
         L     R1,ANETWS1          PASS CLIENT RECORD IN W/S AREA 1             
         ST    R1,NBACLI                                                        
         EJECT                                                                  
*              INITIALIZE                                                       
         SPACE 3                                                                
AE2      MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         CLI   OFFLINE,C'Y'                                                     
         BNE   AE3                 ALLOW FOR DDS REQUEST                        
         CLC   SPLCLI(3),=C'***'                                                
         BNE   AE3                                                              
         XC    NBSELAGY,NBSELAGY                                                
         XC    NBEFFAGY,NBEFFAGY                                                
         MVC   SPLCLI(3),=C'ALL'                                                
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 3                                                                
AE3      MVI   FTERMFLG,0          FIELD IS REQUIRED                            
         LA    R2,SPLCLIH                                                       
         NETGO NVCLIALL,DMCB,SPLCLIN   CLIENT. ALL ALLOWED                      
         OI    SPLCLINH+6,X'80'    XMIT CLI NAME                                
*                                                                               
         LA    R2,SPLPROH                                                       
******   CLI   9(R2),C'='          IF GROUP THEN FILL NBSELPGR                  
******   BE    VGROUP                                                           
         MVC   NBSELPRD,=C'ALL'    PRESET TO ALL FOR PROD GROUP                 
         CLI   9(R2),C'='          IF GROUP THEN IGNORE                         
         BE    VEST                                                             
*                                                                               
         MVC   NBSELPRD,8(R2)      PREFILL NBSELPRD                             
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VEST                                                             
         NETGO NVPRD,DMCB,SPLPRON  PRODUCT                                      
         OI    SPLPRONH+6,X'80'    XMIT PRODUCT NAME                            
         B     VEST                                                             
*                                                                               
VGROUP   NETGO NVGETFLD,DMCB       FOR PRODUCT GROUP                            
         CLC   FLD+2(3),=C'ALL'    FOR X=ALL, IGNORE                            
         BE    VEST                                                             
         MVC   NBSELPGR(1),FLD        REMOVE EQUAL SIGN  (=)                    
         MVC   NBSELPGR+1(4),FLD+2                                              
*                                                                               
VEST     LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK  RANGE OF ESTS.                   
         OI    SPLESTNH+6,X'80'     XMIT EST NAME                               
*                                                                               
*****    LA    R2,SPLEFLTH         ESTIMATE FILTER                              
*****    NETGO NVEFILT,DMCB                                                     
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING ARE OPTIONAL                       
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN      DAYPART                                  
         OI    SPLDPTNH+6,X'80'         XMIT DAYPART NAME                       
*                                                                               
         LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN   PACKAGE                                  
         OI    SPLPAKNH+6,X'80'        XMIT PACKAGE NAME                        
*                                                                               
EDB      LA    R2,SPLFLAVH                                                      
         LA    R3,FLAVLIST                                                      
         BAS   RE,LISTVAL                                                       
         LA    R2,SPLFORMH                                                      
         BAS   RE,FORMVAL                                                       
         LA    R2,SPLDETH                                                       
         LA    R3,DETLIST                                                       
         LA    R4,DETMENU                                                       
         BAS   RE,MULTVAL                                                       
         LA    R2,SPLSUBH                                                       
         LA    R3,SUBLIST                                                       
         SR    R4,R4                                                            
         BAS   RE,MULTVAL                                                       
         LA    R2,SPLRECH                                                       
         LA    R3,RECLIST                                                       
         LA    R4,RECMENU                                                       
         BAS   RE,MULTVAL                                                       
         LA    R2,SPLRSUBH                                                      
         LA    R3,SUBLIST2                                                      
         SR    R4,R4                                                            
         BAS   RE,MULTVAL                                                       
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
ED3B     LA    R2,SPLDEMH                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
ED4      LA    R2,SPLOTHH                                                       
         BAS   RE,VALIOTH                                                       
*                                                                               
         LA    R2,SPLFILTH                                                      
         NETGO NVFILT,DMCB                                                      
*                                                                               
         LA    R2,SPLALLUH         ALLUNITS?                                    
         NETGO NVGETFLD,DMCB                                                    
*                                                                               
         XC    PCTG,PCTG           DEFAULT PCTG TO 0                            
         LA    R2,SPLPCTH          PCTG                                         
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED8                                                              
         LR    R3,R1               (R1 IS LENGTH OF FIELD)                      
         GOTO1 CASHVAL,DMCB,FLD,(R3)                                            
         CLI   DMCB,X'FF'          CHECK FOR ERROR                              
         BE    EDERR                                                            
         MVC   PCTG,DMCB+4         SAVE VALUE IN PCTG                           
*                                                                               
ED8      LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 2                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              VALIDATION OF CLIENT                                             
*              ROUTINE TO VALIDATE AGAINST A LIST                               
         SPACE 2                                                                
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
FLAVLIST DC    C'EVP',X'FF'                                                     
DETLIST  DC    C'CGBNEPDYTLS-',X'FF'                                            
SUBLIST  DC    C'PMWQ4-',X'FF'                                                  
RECLIST  DC    C'CGBNEMQW4S-',X'FF'                                             
SUBLIST2 DC    C'MQ4-',X'FF'                                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE MULTIPLE LIST                                
         SPACE 3                                                                
MULTVAL  NTR1                                                                   
         LTR   R4,R4               R4=A(OPTIONAL MENU)                          
         BZ    MULTB                                                            
         MVI   0(R4),0                                                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   8(R2),X'F0'         THEN NUMERIC=MENU NO                         
         BL    MULTB                                                            
         GOTO1 VALINUM                                                          
         MVC   0(1,R4),ACTUAL                                                   
         B     XIT                                                              
         SPACE 1                                                                
MULTB    CLI   8(R2),C'-'                                                       
         BE    XIT                                                              
         LA    R4,8(R2)                                                         
         ZIC   R6,5(R2)                                                         
         LTR   R6,R6                                                            
         BZ    XIT                                                              
         MVC   DUMLIST,0(R3)                                                    
         LA    R3,DUMLIST                                                       
         SPACE 1                                                                
MULT2    LR    R5,R3                                                            
         CLI   0(R4),C','          IGNORE COMMAS                                
         BE    MULT6                                                            
         SPACE 1                                                                
MULT4    CLC   0(1,R4),0(R5)                                                    
         BE    MULT5                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   MULT4                                                            
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 1                                                                
MULT5    MVI   0(R5),0             TAKE OUT OF LIST TO AVOID DUPS.              
         SPACE 1                                                                
MULT6    LA    R4,1(R4)                                                         
         BCT   R6,MULT2                                                         
         XC    DUMLIST,DUMLIST                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE FORMAT                                       
         SPACE 3                                                                
FORMVAL  NTR1                                                                   
         MVI   MENU,0                                                           
         XC    COLS,COLS                                                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   8(R2),C'-'                                                       
         BE    XIT                                                              
         CLI   8(R2),X'F0'                                                      
         BL    FORM2                                                            
         GOTO1 VALINUM                                                          
         MVC   MENU,ACTUAL         NUMERIC IS MENU                              
         B     XIT                                                              
         SPACE 1                                                                
FORM2    GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK            INTERPRET BLOCK                              
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4               MUST BE SOME                                 
         BZ    FORMEX                                                           
         LA    R5,COLS                                                          
         SPACE 1                                                                
FORM4    ZIC   R1,0(R3)                                                         
         LTR   R1,R1               GAPS ARE ALLOWED                             
         BZ    FORM12                                                           
         BCTR  R1,0                                                             
         LA    R6,COLTAB                                                        
         CLI   SPLFLAV,C'E'                                                     
         BE    FORM6                                                            
         LA    R6,PCOLTAB                                                       
         SPACE 1                                                                
FORM6    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),0(R6)                                                   
         BE    FORM8                                                            
         LA    R6,12(R6)                                                        
         CLI   0(R6),X'FF'                                                      
         BE    FORMEX                                                           
         B     FORM6                                                            
         SPACE 1                                                                
FORM8    CLC   SPLFLAV(1),9(R6)    MUST BE SUPPORTED FOR FLAVOR                 
         BE    FORM10                                                           
         CLC   SPLFLAV(1),10(R6)                                                
         BE    FORM10                                                           
         CLC   SPLFLAV(1),11(R6)                                                
         BNE   FORMEX                                                           
         SPACE 1                                                                
FORM10   MVC   0(1,R5),8(R6)       COLUMN NUMBER                                
         SPACE 1                                                                
FORM12   LA    R3,32(R3)                                                        
         LA    R5,1(R5)                                                         
         BCT   R4,FORM4                                                         
         B     XIT                                                              
         SPACE 1                                                                
FORMEX   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE OTHERS                                       
         SPACE 3                                                                
VALIOTH  NTR1                                                                   
         MVI   SPACOPT,1                                                        
         MVI   COMMOPT,C'N'                                                     
         MVI   SEPOPT,C'N'                                                      
         MVI   TOGOPT,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    FORMEX                                                           
         SPACE 1                                                                
OTH2     CLC   12(4,R3),=C'SEP '                                                
         BNE   OTH3                                                             
         MVI   SEPOPT,C'Y'                                                      
         B     OTH8                                                             
OTH3     CLI   12(R3),C'S'         SPACING                                      
         BNE   OTH4                                                             
         MVC   SPACOPT,11(R3)                                                   
         CLI   SPACOPT,0                                                        
         BE    FORMEX                                                           
         CLI   SPACOPT,3                                                        
         BH    FORMEX                                                           
         B     OTH8                                                             
         SPACE 1                                                                
OTH4     CLC   12(3,R3),=C'COM'    COMMENT OPTION                               
         BNE   OTH6                                                             
         MVC   COMMOPT,22(R3)                                                   
         CLI   COMMOPT,C'Y'                                                     
         BE    OTH8                                                             
         B     FORMEX                                                           
         SPACE 1                                                                
OTH6     CLC   12(3,R3),=C'BOX'     BOX PRINTING OPTION                         
         BNE   OTH7                                                             
         MVI   BOXOPT,C'Y'                                                      
         B     OTH8                                                             
OTH7     CLC   12(3,R3),=C'TOG'    CLIENTS TOGETHER OPTION                      
         BNE   OTH8                                                             
         MVI   TOGOPT,C'Y'                                                      
         MVI   NBCLITOG,C'T'                                                    
         B     OTH8                                                             
         SPACE 1                                                                
OTH8     LA    R3,32(R3)                                                        
         BCT   R4,OTH2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              TABLE OF VALID COLUMNS                                           
         SPACE 3                                                                
COLTAB   DS    0H                                                               
         DC    C'0       ',AL1(00),C'EPV'                                       
         DC    C'UNITS   ',AL1(25),C'EPV'                                       
         DC    C'ASSIGNED',AL1(01),C'E  '                                       
         DC    C'AS+     ',AL1(02),C'E  '                                       
         DC    C'ASS+    ',AL1(02),C'E  '                                       
         DC    C'ACTUAL  ',AL1(03),C'E  '                                       
         DC    C'AC+     ',AL1(04),C'E  '                                       
         DC    C'ACT+    ',AL1(04),C'E  '                                       
         DC    C'GROSS   ',AL1(05),C'E  '                                       
         DC    C'DIFF    ',AL1(06),C'E  '                                       
         DC    C'INT     ',AL1(07),C'E  '                                       
         DC    C'CUT-IN  ',AL1(08),C'E  '                                       
         DC    C'TOTAL   ',AL1(20),C'E  '                                       
         DC    C'TOTG    ',AL1(09),C'E  '                                       
         DC    C'TOTN    ',AL1(10),C'E  '                                       
         DC    C'LIST    ',AL1(11),C'E  '                                       
         DC    C'COMM    ',AL1(12),C'E  '                                       
         DC    C'NET     ',AL1(13),C'E  '                                       
         DC    C'NET+    ',AL1(14),C'E  '                                       
         DC    C'CLEARED ',AL1(15),C'E  '                                       
         DC    C'PD      ',AL1(15),C'E  '                                       
         DC    C'PAID    ',AL1(15),C'E  '                                       
         DC    C'UNCLEAR ',AL1(16),C'E  '                                       
         DC    C'UNPAID  ',AL1(16),C'E  '                                       
         DC    C'BILLED  ',AL1(17),C'E  '                                       
         DC    C'BILLABLE',AL1(18),C'E  '                                       
         DC    C'UNBILLED',AL1(18),C'E  '                                       
         DC    C'NIN     ',AL1(19),C'E  '                                       
         SPACE 1                                                                
PCOLTAB  DC    C'UNITS   ',AL1(25),C'EPV'                                       
         DC    C'GAP     ',AL1(00),C'EPV'                                       
         DC    C'HUT     ',AL1(31),C'VP '                                       
         DC    C'SHARE   ',AL1(32),C'VP '                                       
         DC    C'RTG     ',AL1(33),C'VP '                                       
         DC    C'$       ',AL1(34),C'VP '                                       
         DC    C'COST    ',AL1(34),C'VP '                                       
         DC    C'ACOST   ',AL1(35),C'VP '                                       
         DC    C'IH      ',AL1(40),C'VP '                                       
         DC    C'RH      ',AL1(41),C'VP '                                       
         DC    C'I1      ',AL1(42),C'VP '                                       
         DC    C'R1      ',AL1(43),C'VP '                                       
         DC    C'V1      ',AL1(44),C'VP '                                       
         DC    C'I2      ',AL1(45),C'VP '                                       
         DC    C'R2      ',AL1(46),C'VP '                                       
         DC    C'V2      ',AL1(47),C'VP '                                       
         DC    C'I3      ',AL1(48),C'VP '                                       
         DC    C'R3      ',AL1(49),C'VP '                                       
         DC    C'V3      ',AL1(50),C'VP '                                       
         DC    C'VH      ',AL1(51),C'VP '                                       
         DC    C'IRH     ',AL1(51),C'VP '                                       
         DC    C'CPPH    ',AL1(52),C'VP '                                       
         DC    C'CPP1    ',AL1(53),C'VP '                                       
         DC    C'CPP2    ',AL1(54),C'VP '                                       
         DC    C'CPP3    ',AL1(55),C'VP '                                       
         DC    C'CPMH    ',AL1(56),C'VP '                                       
         DC    C'CPM1    ',AL1(57),C'VP '                                       
         DC    C'CPM2    ',AL1(58),C'VP '                                       
         DC    C'CPM3    ',AL1(59),C'VP '                                       
         DC    C'VPH1    ',AL1(60),C'VP '                                       
         DC    C'VPH2    ',AL1(61),C'VP '                                       
         DC    C'VPH3    ',AL1(62),C'VP '                                       
         DC    C'IR1     ',AL1(63),C'VP '                                       
         DC    C'IR2     ',AL1(64),C'VP '                                       
         DC    C'IR3     ',AL1(65),C'VP '                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR EDIT                                                   
         SPACE 3                                                                
EDD      DSECT                     COMMON WITH PRINT                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                                                               
DPFILT   DS    CL1                                                              
MENU     DS    CL1                                                              
COLS     DS    CL10                                                             
DETMENU  DS    CL1                                                              
RECMENU  DS    CL1                                                              
SPACOPT  DS    CL1                                                              
COMMOPT  DS    CL1                                                              
SEPOPT   DS    CL1                                                              
BOXOPT   DS    CL1                                                              
TOGOPT   DS    CL1                 CLIENTS TOGETHER                             
PCTG     DS    F                   PERCENTAGE ADJUSTMENT                        
DUMLIST  DS    CL16                                                             
         SPACE 3                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF6D                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NEMED06   05/01/02'                                      
         END                                                                    
