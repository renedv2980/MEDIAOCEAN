*          DATA SET NEMED46    AT LEVEL 025 AS OF 08/10/00                      
*PHASE T31E46A                                                                  
*                                                                               
         TITLE 'T31E46 - EDIT FOR POST BUY'                                     
*****************************************************************               
* T31E46 (NEMED46) - THIS EDITS THE POST BUY SCREEN.                            
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS             
*                           MANY USEFUL ADDRESSES, DATA                         
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.                 
*                      MANY FIELDS FILLED IN BY NETIO.                          
*           IO1 - FIRST W/S AREA. CONTAINS THE CLIENT RECORD.                   
*                  PASSED TO PRINT MODULE.                                      
*           NET DEMO BLOCK - NDDEMOS,NDNDEMOS - DEFAULT FROM                    
*                         ESTIMATE RECORDS. OVERRIDDEN BY INPUT.                
*           FLAVOR - FLAVOR SELECTED. DEFAULT X'40'                             
*                                                                               
* GLOBALS - R2 - POINTS TO CURRENT FIELD ON SCREEN                              
*                                                                               
*  CALLS TO -                                                                   
*   NVVALID - VALIDATION ROUTINES.                                              
*                                                                               
* ORGANIZATION OF WORKING STORAGE FOR POST REPORT:                              
*       (PRINT MODULE MUST USE SAME ORGANIZATION)                               
*                  *******************************                              
*  ANETWS1 ->      * WORKING STORAGE BLOCK 1     *                              
*                  * ---- CLIENT RECORD          *                              
*  ANETWS2 -> R7-> * WORKING STORAGE BLOCK 2     *                              
*                  * ---- NET DEMO BLOCK         *                              
*                  * ---- DEMO BLOCK             *                              
*                  * ---- ARGUMENTS TO PASS      *                              
*                  * ------ FLAVOR               *                              
*                  * ------ DATEOPT              *                              
*                  * ------ REROPT               *                              
*                  * ---- LOCAL W/S              *                              
*                  *******************************                              
*                                                                               
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
NEMED46  CSECT                                                                  
         NMOD1 0,**POED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          USE 1ST W/S AREA TO PASS CLIENT              
         ST    R2,NBACLI           I/O AREA TO SAVE CLIENT RECORD               
*                                    TO PASS TO PRINT MODULE                    
         L     R7,ANETWS2          USE W/S AREA 2 FOR NET DEMO BLOCK            
         ST    R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
*                                                                               
         LA    R2,TWNCLIH          CLIENT (REQUIRED. NO SPEC CASES)             
         NETGO NVCLI,DMCB,TWNCLIN      AND FILL IN TWNCLIN.                     
*                                  (RETURNS CLIENT RECORD AT AIO2)              
         OI    TWNCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
         LA    R2,TWNPROH          PRODUCT (REQUIRED. NO SPEC CASES)            
         NETGO NVPRD,DMCB,TWNPRON      AND FILL IN TWNPRON.                     
         OI    TWNPRONH+6,X'80'    TRANSMIT PRON                                
*                                                                               
         LA    R2,TWNESTH          ESTIMATE (REQUIRED. NO SPEC CASES)           
         NETGO NVESTRNG,DMCB,TWNESTN,NDDEMBLK    FILL IN TWNESTN, DEMOS         
         NETGO NVDELHOM,DMCB,NDDEMOS     REMOVE HOMES FROM DEMOLIST.            
         OI    TWNESTNH+6,X'80'    TRANSMIT ESTN                                
*                                                                               
         MVI   FTERMFLG,1          OPTIONAL. NO RESPONSE=ALL                    
         LA    R2,TWNNETH          NETWORK ( 'ALL' ALLOWED)                     
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,TWNDPTH          DAYPART ( DEFAULT ALL)                       
         NETGO NVDPT,DMCB,TWNDPTN    OPTIONAL. FILL IN TWNDPTN                  
         OI    TWNDPTNH+6,X'80'    TRANSMIT DPTN                                
*                                                                               
         LA    R2,TWNPAKH          PACKAGE (OPTIONAL)                           
         NETGO NVPAKLOK,DMCB,TWNPAKN  AND FILL IN TWNPAKN.                      
         OI    TWNPAKNH+6,X'80'    TRANSMIT PAKN                                
*                                                                               
         LA    R2,TWNFLAVH         FLAVOR. OPTIONAL.                            
         NETGO NVGETFLD,DMCB                                                    
         MVC   FLAVOR,FLD          SAVE IT TO PASS TO PRINT MODULE.             
         CLI   FLD,C'C'                                                         
         BE    DATOPT                                                           
         MVI   FLAVOR,C'A'            DEFAULT TO AUDIT TRAIL                    
         B     DATOPT                                                           
*                                                                               
DATOPT   MVI   FTERMFLG,1                                                       
         LA    R2,TWNDOPTH         DATE OPTION. OPTIONAL.                       
         NETGO NVGETFLD,DMCB                                                    
         MVC   DATEOPT,FLD         SAVE IT FOR PRINT MODULE                     
         CLI   FLD,C'W'                                                         
         BE    DEMS                                                             
         MVI   DATEOPT,C'M'        DEFAULT TO MONTH                             
*                                                                               
DEMS     MVI   FTERMFLG,1          OPTIONAL                                     
         LA    R2,TWNDEMH                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK   PUT DEMOS IN BLOCK IF GIVEN         
*                                                                               
         LA    R2,TWNSTRTH         START DATE (OPTIONAL)                        
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,TWNENDH          END DATE. (OPTIONAL). ALSO INSURES           
         NETGO NVENDDAT,DMCB        END DATE IS NOT BEFORE START                
*                                                                               
         LA    R2,TWNOPTH          OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    ENDOPT                                                           
         GOTO1 SCANNER,DMCB,(R2),(4,BLOCK)                                      
         ZIC   R0,DMCB+4                                                        
         LA    R3,BLOCK                                                         
OPT1     CLC   12(5,R3),=C'AFFID'  AFFIDAVIT TIME OPTION                        
         BNE   OPT5                     SETS NBAFFOPT                           
         CLI   22(R3),C'Y'                                                      
         BE    OPT1B                                                            
         CLI   22(R3),C'N'                                                      
         BNE   EDERR                                                            
OPT1B    MVC   AFFIDOPT,22(R3)                                                  
         B     ENDOPT                                                           
OPT5     LA    R3,32(R3)                                                        
         BCT   R0,OPT1                                                          
         B     EDERR                                                            
ENDOPT   DS    0H                                                               
*                                                                               
RERATE   LA    R2,TWNRERH          OPTIONAL RERATE                              
         MVI   REROPT,0            DEFAULT IS NO                                
         CLI   5(R2),0                                                          
         BE    ENDRER                                                           
         MVC   REROPT,8(R2)                                                     
         CLI   REROPT,C'D'         MUST BE D(IARY)                              
         BE    ENDRER                                                           
         CLI   REROPT,C'C'         OR C(ONFORMED)                               
         BE    ENDRER                                                           
         CLI   REROPT,C'I'         OR I(TEGRATED)                               
         BE    ENDRER                                                           
         CLI   REROPT,C'A'         OR A(SCRIBED)                                
         BE    ENDRER                                                           
         MVI   REROPT,0                                                         
ENDRER   DS    0H                                                               
*                                                                               
ENDPOST  LA    R2,TWNCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
*                                                                               
EDERR    MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
***  INCLUDE NETINCLS *******                                                   
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE6D                                                       
*                                                                               
         DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
**                                                                              
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
***      ARGS TO POST                                                           
FLAVOR   DS    CL1                                                              
DATEOPT  DS    CL1                                                              
REROPT   DS    CL1                                                              
AFFIDOPT DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NEMED46   08/10/00'                                      
         END                                                                    
