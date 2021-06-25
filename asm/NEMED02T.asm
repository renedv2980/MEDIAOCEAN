*          DATA SET NEMED02T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED02    AT LEVEL 035 AS OF 02/17/95                      
*PHASE T31E02A,+0                                                               
         TITLE 'T31E02 - EDIT FOR WEEKLY FLOWCHART'                             
T31E02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**WEED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*     INITIALIZE NETBLOCK                                                       
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         L     R1,ANETWS1                                                       
         ST    R1,NBCNVNTI                                                      
         L     R7,ANETWS3                                                       
         ST    R7,NBADEM           USE W/S AREA 3 FOR NET DEMO BLOCK            
         USING NDDEMBLK,R7                                                      
*                                                                               
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
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK    ALLOWS RANGE OF ESTS           
         L     R1,NBAIO                                                         
         USING ESTHDR,R1                                                        
         MVC   NDDEMOS(60),EDEMLST                                              
         MVC   NDDEMOS+60(3),EDEM21                                             
         MVC   NDWGTLST+20(1),EDEM21WT                                          
*                                                 AND ALL, NO                   
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
         LA    R2,SPLSEQH          SEQUENCE OPTION                              
         NETGO NVPSEQ,DMCB                                                      
*                                                                               
EDB      LA    R2,SPLFLAVH                                                      
         MVI   FLAVOR,C'E'                                                      
         CLI   5(R2),0                                                          
         BE    EDD                                                              
         MVC   FLAVOR,8(R2)                                                     
         LA    R3,FLAVLIST                                                      
         BAS   RE,LISTVAL                                                       
*                                                                               
EDD      LA    R2,SPLDOPTH                                                      
         MVI   DATOPT,C'W'                                                      
         CLI   5(R2),0                                                          
         BE    EDE                                                              
         MVC   DATOPT,8(R2)                                                     
         LA    R3,DOPTLIST                                                      
         BAS   RE,LISTVAL                                                       
*                                                                               
EDE      LA    R2,SPLSPOTH         Y=BY SPOTS,L=BY SPTLEN,B=BOTH                
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDEE                                                             
         MVI   SPTLEN,C'Y'                                                      
         MVI   MYSPOT,C'Y'                                                      
         CLI   FLD,C'B'                                                         
         BE    EDEE                                                             
         CLI   FLD,C'Y'                                                         
         BNE   *+12                                                             
         MVI   SPTLEN,0                                                         
         B     EDEE                                                             
         CLI   FLD,C'L'                                                         
         BNE   EDERR                                                            
         MVI   MYSPOT,0                                                         
*                                                                               
EDEE     LA    R2,SPLGRPTH                                                      
         MVI   GRPOPT,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    EDG                                                              
         MVC   GRPOPT,8(R2)                                                     
*                                                                               
EDG      LA    R2,SPLSTRTH                                                      
         CLI   DATOPT,C'D'         IF DAYS OPTION                               
         BNE   *+8                                                              
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         NETGO NVSTRDAT,DMCB       START DATE.                                  
*                                                                               
ED2      LA    R2,SPLENDH          END DATE.                                    
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         CLI   DATOPT,C'D'          IF DAYS OPTION                              
         BNE   ED2D                                                             
         NETGO NVGETFLD,DMCB                 IF NO END DATE                     
         BZ    ED2B                                                             
         NETGO NVENDDAT,DMCB,15              ELSE 15 DAYS MAX SPREAD            
         B     ED2E                                                             
ED2B     GOTO1 ADDAY,DMCB,NBSELSTR,NBSELEND,13                                  
         GOTO1 DATCON,DMCB,NBSELEND,(5,SPLEND)                                  
         MVI   SPLENDH+5,8                                                      
         OI    SPLENDH+6,X'80'                                                  
ED2D     NETGO NVENDDAT,DMCB       ALSO CHECKS START NOT GT END                 
*                                                                               
ED2E     LA    R2,SPLDEMH                                                       
**       NETGO NVGETFLD,DMCB       GOTO NVDEM SO IT SETS UP DBLOCK              
**       BZ    EDGOAL              FOR DEMOCON CALL AT ED8                      
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
EDGOAL   LA    R2,SPLGOALH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED8                                                              
         CLI   FLD,C'Y'                                                         
         BE    ED8                                                              
         CLI   FLD,C'N'                                                         
         BE    ED8                                                              
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
ED8      LA    R2,SPLCLIH                                                       
         NETGO NVDEMCON,DMCB,(0,NDDEMBLK),DBLOCK,(7,DEMNMSV)                    
*                                                                               
         MVC   MYFLAV,SPLFLAV                                                   
         MVC   MYUNIT,SPLUNIT                                                   
         MVC   MYGOAL,SPLGOAL                                                   
         MVC   MYNOPT,SPLNOPT                                                   
*                                                                               
         B     XMOD                                                             
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE AGAINST A LIST                               
*                                                                               
LISTVAL  NTR1                                                                   
*                                                                               
LISTVAL2 CLC   0(1,R3),8(R2)                                                    
         BE    XIT                                                              
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   LISTVAL2                                                         
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
FLAVLIST DC    C'EA',X'FF'                                                      
DOPTLIST DC    C'MWD',X'FF'                                                     
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF2D                                                       
*                                                                               
         DSECT                     W/S                                          
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
* LOCAL W/S. ALSO PASSED TO EDIT.                                               
DPFILT   DS    CL1                                                              
FLAVOR   DS    CL1                                                              
DATOPT   DS    CL1                                                              
GRPOPT   DS    CL1                                                              
DFSOPT   DS    CL1                                                              
DEMNMSV  DS    CL7                                                              
         DS    CL5                 DO NOT USE DEMNMSV USES THIS                 
SPTLEN   DS    CL1                                                              
MYFLAV   DS    CL2                                                              
MYSPOT   DS    CL1                                                              
MYUNIT   DS    CL1                                                              
MYGOAL   DS    CL1                                                              
MYNOPT   DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED02T  05/01/02'                                      
         END                                                                    
