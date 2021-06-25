*          DATA SET NEMED07    AT LEVEL 069 AS OF 03/19/20                      
*PHASE T31E07B,+0                                                               
*                                                                               
         TITLE 'T31E07 - EDIT FOR NTI PKTPIECE-UNIT SEED'                       
**********************************************************************          
* NEMED07(T31E07) - THIS EDITS NTI PKTPC-UNT SEED REPORT SCREEN      *          
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS  *          
*                           MANY USEFUL ADDRESSES, DATA              *          
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.      *          
*                      MANY FIELDS FILLED IN BY NETIO.               *          
*                                                                               
*                                                                               
*  CALLS TO -                                                        *          
*   NVVALID - VALIDATION ROUTINES.                                   *          
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
T31E07   CSECT                                                                  
         NMOD1 0,**SDED**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
         LA    RE,MYWORKD                                                       
         LA    RF,1000                                                          
         XCEF                                                                   
         ST    R2,RELO                                                          
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
*                                    TO PASS TO PRINT MODULE                    
*                                                                               
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLIALL,DMCB,SPLCLIN      FILL IN NAME                          
         OI    SPLCLINH+6,X'80'           TRANSMIT NAME                         
*                                                                               
         LA    R2,SPLPROH          PRODUCT                                      
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         NETGO NVPRDALL,DMCB,SPLPRON      AND FILL IN NAME                      
         CLC   =C'ALL',SPLCLI      IF CLI=ALL SKIP PRDNM FOUT                   
         BE    *+8                 SINCE YOU GET GARBAGE                        
         OI    SPLPRONH+6,X'80'           TRANSMIT PRODUCT NAME                 
         CLI   OFFLINE,C'Y'                                                     
         BE    *+14                                                             
         CLC   NBSELPRD,=C'ALL'    NO PRD=ALL ON-LINE REQUESTS                  
         BE    EDINV               EITHER POL OR A SINGLE PRODUCT               
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         CLC   CONWHEN(4),=C'SOON'                                              
         BNE   EDT5                                                             
         MVI   FTERMFLG,0                 SET REQUIRED FLAG IF SOON             
         NETGO NVEST,DMCB,SPLESTN                                               
         B     EDT5A                                                            
*                                                                               
EDT5     MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         NETGO NVESTRNG,DMCB,SPLESTN      AND FILL IN NAME                      
EDT5A    OI    SPLESTNH+6,X'80'           TRANSMIT NAME                         
*                                                                               
         LA    R2,SPLNETH          NETWORK                                      
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         NETGO NVNETALL,DMCB,SPLNETN      AND FILL IN NAME                      
         OI    SPLNETNH+6,X'80'           TRANSMIT NAME                         
*                                                                               
         LA    R2,SPLDPTH          DAYPART                                      
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         NETGO NVDPTALL,DMCB,SPLDPTN      AND FILL IN NAME                      
         OI    SPLDPTNH+6,X'80'           TRANSMIT NAME                         
*                                                                               
         LA    R2,SPLPAKH          PACKAGE                                      
         CLC   SPLPAK(3),=C'ALL'                                                
         BE    EDT08                                                            
         MVI   FTERMFLG,1               SET OPTIONAL FLAG                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT08                                                            
         NETGO NVPAKLOK,DMCB,SPLPAKN     ELSE VALIDATE/FILL IN SPLPAKN          
         OI    SPLPAKNH+6,X'80'       TRANSMIT PAKN                             
*                                                                               
EDT08    DS    0H                                                               
         CLC   CONWHEN(4),=C'SOON'                                              
         BNE   *+8                                                              
         MVI   FTERMFLG,0          IF SOON, DATES REQUIRED                      
*                                                                               
         LA    R2,SPLSCDH                                                       
         CLI   5(R2),0                                                          
         JE    EDT09                                                            
         CLI   SPLSCD,C'Y'         SEED COMSCORE?                               
         BNE   EDT08A                                                           
*        MVI   REQSML,C'C'         TURN OFF COMSCORE - ITMF-44950               
*        TM    WHEN,WOK$OV         OVERNIGHT REQUEST?                           
*        JZ    *+8                                                              
*        OI    GENSTAT7,GES7COMS                                                
         B     EDT09                                                            
EDT08A   CLI   SPLSCD,C'N'                                                      
         BE    EDT09                                                            
         CLI   SPLSCD,X'40'                                                     
         BH    EDINV                                                            
         B     EDT09                                                            
EDT09    DS    0C                                                               
*                                                                               
         CLC   =C'AUTOSEED',CONACT  IS THIS AUTOSEED REPORT ?                   
         BE    EDTAUTO                                                          
         CLC   =C'ONEDSEED',CONACT  IS THIS ONE DAY BEFORE SEED ?               
         BE    EDTONED                                                          
         CLC   =C'ONEWSEED',CONACT  IS THIS ONE WEEK SEED ?                     
         BE    EDTONEW                                                          
         CLC   =C'THRDSEED',CONACT  IS THIS 3D DAY BEFORE SEED ?                
         BE    EDTONED                                                          
         B     EDT10AA                                                          
**************************************************************                  
* AUTOSEED REPORT RUN ON THURS ONLY                                             
* GET MON-SUN DATES OF 2 WEEKS PREVIOUS                                         
                                                                                
EDTAUTO  CLI   OFFLINE,C'Y'        ONLY DO DATES IF OFFLINE                     
         BE    EDTAUT5             ELSE/SKIP DATE ROUTINE                       
         B     EDT20                                                            
**       CLC   =C'FILE',CONOUT                                                  
**       BE    EDT20                                                            
                                                                                
*                                                                               
EDTAUT5  GOTO1 DATCON,DMCB,(5,WORK),(0,BLOCK)                                   
         GOTO1 GETDAY,DMCB,BLOCK,BLOCK+6                                        
         CLI   DMCB,4                      MUST BE THURSDAY                     
         BE    DATEOK                                                           
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'ERROR - MUST BE THURSDAY REQUEST'                 
         OI    CONHEADH+6,X'80'    TRANSMIT                                     
         LA    R2,SPLCLIH          CLIENT                                       
         OI    6(R2),X'40'         POSITION CURSOR/TRANSMIT                     
         GOTO1 ERREX2                                                           
*                                                                               
DATEOK   XC    SPLSTRT,SPLSTRT                                                  
         XC    SPLEND,SPLEND                                                    
         GOTO1 ADDAY,DMCB,BLOCK,BLOCK+6,F'-17'      MON 2 WEEKS PREV            
         GOTO1 DATCON,DMCB,(0,BLOCK+6),(5,SPLSTRT)                              
         MVI   SPLSTRTH+5,8                SET INPUT LENGTH                     
         GOTO1 ADDAY,DMCB,BLOCK+6,BLOCK+12,F'6'       SUN 2 WEEKS PREV          
         GOTO1 DATCON,DMCB,(0,BLOCK+12),(5,SPLEND)                              
         MVI   SPLENDH+5,8         SET INPUT LENGTH                             
         B     EDT10AA             DO NORMAL PROCESSING                         
******************************************************************              
**************************************************************                  
* ONEDSEED REPORT RUNS FOR PREVIOUS DAY                                         
* THRDSEED REPORT RUNS FOR 3D DAY PREVIOUS                                      
                                                                                
EDTONED  CLI   OFFLINE,C'Y'        ONLY DO DATES IF OFFLINE                     
         BE    EDTONE5             ELSE/SKIP DATE ROUTINE                       
         B     EDT20                                                            
         CLC   =C'FILE',CONOUT                                                  
         BE    EDT20                                                            
*                                                                               
EDTONE5  GOTO1 DATCON,DMCB,(5,WORK),(0,BLOCK)                                   
         GOTO1 GETDAY,DMCB,BLOCK,BLOCK+6                                        
         XC    SPLSTRT,SPLSTRT                                                  
         XC    SPLEND,SPLEND                                                    
         GOTO1 ADDAY,DMCB,BLOCK,BLOCK+6,F'-1'      PREV DAY                     
         CLC   =C'ONEDSEED',CONACT                                              
         BE    EDTONE6                                                          
         GOTO1 ADDAY,DMCB,BLOCK,BLOCK+6,F'-3'      ASSUME 3D DAY BEFORE         
EDTONE6  GOTO1 DATCON,DMCB,(0,BLOCK+6),(5,SPLSTRT)                              
         MVI   SPLSTRTH+5,8                SET INPUT LENGTH                     
         MVC   SPLEND,SPLSTRT                                                   
         MVI   SPLENDH+5,8         SET INPUT LENGTH                             
         B     EDT10AA             DO NORMAL PROCESSING                         
******************************************************************              
**************************************************************                  
* RUNS PREVIOUS MON-SUNDAY                                                      
EDTONEW  CLI   OFFLINE,C'Y'        ONLY DO DATES IF OFFLINE                     
         BE    EDTONEW5            ELSE/SKIP DATE ROUTINE                       
         B     EDT20                                                            
         CLC   =C'FILE',CONOUT                                                  
         BE    EDT20                                                            
*                                                                               
EDTONEW5 GOTO1 DATCON,DMCB,(5,WORK),(0,BLOCK)                                   
         XC    SPLSTRT,SPLSTRT                                                  
         XC    SPLEND,SPLEND                                                    
         GOTO1 ADDAY,DMCB,BLOCK,BLOCK+6,F'-13'    BUMP BACK 13 DAYS             
EDTONEW7 GOTO1 GETDAY,DMCB,BLOCK+6,WORK                                         
         CLI   DMCB,1                      NEED MONDAY                          
         BE    EDTONEW9                                                         
         GOTO1 ADDAY,DMCB,BLOCK+6,BLOCK+6,F'1'    BUMP FORWARD 1 DAY            
         B     EDTONEW7                                                         
*                                                                               
EDTONEW9 GOTO1 DATCON,DMCB,(0,BLOCK+6),(5,SPLSTRT)                              
         MVI   SPLSTRTH+5,8                          SET INPUT LENGTH           
         GOTO1 ADDAY,DMCB,BLOCK+6,BLOCK+12,F'6'       NEED SUNDAY               
         GOTO1 DATCON,DMCB,(0,BLOCK+12),(5,SPLEND)                              
         MVI   SPLENDH+5,8         SET INPUT LENGTH                             
         B     EDT10AA             DO NORMAL PROCESSING                         
******************************************************************              
         EJECT                                                                  
*                                                                               
EDT10AA  LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB          START DATE                                
*                                                                               
         LA    R2,SPLENDH             END DATE                                  
         CLC   CONWHEN(4),=C'SOON'                                              
         BNE   EDT10A                                                           
         NETGO NVENDDAT,DMCB,115                                                
         B     EDT11                                                            
EDT10A   NETGO NVENDDAT,DMCB                                                    
         EJECT                                                                  
         SPACE                                                                  
* SET UP DEDBLOCK AND CHK TO SEE IF BOOKS ON FILE                               
         SPACE                                                                  
EDT11    MVC   DMCB+4(4),=X'D9000A17'            GET VNETWEEK                   
         B     EDT20                                                            
*                                                                               
* ********8 SKIP TESTING FOR NOW                                                
*                                                                               
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VNETWEEK,DMCB                                                    
         SPACE                                                                  
         L     RE,ACOMFACS                      GET VDEMAND                     
         USING COMFACSD,RE                                                      
         MVC   VDEMAND,CDEMAND                                                  
         SPACE                                                                  
         LA    R2,SPLSTRTH                                                      
         MVC   EBCDAT,NBSELSTR                                                  
EDT12    LA    R5,DBLOCKWS                                                      
         USING DBLOCK,R5                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 VNETWEEK,DMCB,EBCDAT,GETDAY,ADDAY                                
         MVC   DEMOWK,DMCB+8                                                    
         MVC   DEMOYR,DMCB+4                                                    
         SPACE                                                                  
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBAREC,ANETWS1                                                   
         MVI   DBFUNCT,DBVLNBK                                                  
         MVC   DBSELSTA,=C'ABC T'                                               
         MVC   DBSELBK,DEMOYR                                                   
         SPACE                                                                  
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,0                                                        
         BE    EDT17                                                            
         MVC   CONHEAD(37),=C'*** ERROR *** - PKT PIECE NOT ON FILE'            
         OI    CONHEADH+6,X'80'    TRANSMIT                                     
         OI    6(R2),X'C0'         POSITION CURSOR/TRANSMIT                     
         GOTO1 ERREX2                                                           
EDT17    DS    0H                                                               
*        LA    R3,SPLENDH    ***** ONLY CHECKING TO SEE                         
*        CR    R2,R3         ***** IF THERE IS A PKTPIECE FOR START             
*        BE    EDT20         ***** DATE/IF NOT,EST IS INVALID                   
*        LA    R2,SPLENDH                                                       
*        MVC   EBCDAT,NBSELEND                                                  
*        B     EDT12                                                            
         SPACE                                                                  
EDT20    DS    0H                  TEST RUN CHECK                               
         LA    R2,SPLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         CLI   SPLTST,C'Y'                                                      
         BE    EDT30                                                            
         CLI   SPLTST,C'N'                                                      
         BE    EDT25                                                            
         B     EDINV                                                            
EDT25    CLC   CONWHEN(4),=C'SOON'                                              
         BNE   EDT30                                                            
         MVC   CONHEAD(38),=C'** ERROR - SOON ONLY FOR TEST RUN=Y **'           
         GOTO1 ERREX2                                                           
*                                                                               
EDT30    DS    0H                  OPTIONS FIELD                                
         MVI   CORRFLG,0                                                        
         MVI   MLTSDFLG,C'Y'                                                    
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         LA    R2,SPLOPTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDITXX                                                           
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
EDT32    CLC   12(6,R3),=C'RESEED' OPTION TO RESEED UNITS                       
         BNE   *+12                                                             
         MVI   RESEED,C'Y'                                                      
         B     EDT34                                                            
         CLC   12(6,R3),=C'NOMULT' DO NOT SEED ON MULTIPLT NTI CODES            
         BNE   *+12                                                             
         MVI   MLTSDFLG,C'N'                                                    
         B     EDT34                                                            
         CLC   12(3,R3),=C'CORR'    CORRECTIONS ONLY                            
         BNE   *+12                                                             
         MVI   CORRFLG,C'Y'                                                     
         B     EDT34                                                            
         CLC   12(3,R3),=C'SUMRY'   SUMMARY VERSION OF REPORT                   
         BNE   *+12                 ONLY PRINT UNMATCHED/MULTIPLE               
         MVI   SUMMARY,C'Y'                                                     
         B     EDT34                                                            
         CLC   12(4,R3),=C'TYPE'         ASCRIBED DATA OPTION                   
         BNE   EDINV                                                            
         CLI   22(R3),C'A'                                                      
         BNE   EDINV                                                            
         MVI   ASCRIBD,C'Y'                                                     
         SPACE 1                                                                
EDT34    LA    R3,32(R3)                                                        
         BCT   R0,EDT32                                                         
*&&DO                                                                           
EDT40    LA    R2,SPLSCDH                                                       
         CLI   SPLSCD,C'Y'                                                      
         BNE   *+12                                                             
         MVI   REQSML,C'C'                                                      
         B     EDITXX                                                           
         CLI   SPLSCD,C'N'                                                      
         BE    EDITXX                                                           
         CLI   SPLSCD,X'40'                                                     
         BH    EDINV                                                            
         B     EDITXX                                                           
*&&                                                                             
         SPACE 1                                                                
EDITXX   LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
         SPACE                                                                  
*                                                                               
EDINV    MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
XMOD     XIT1                                                                   
         SPACE 2                                                                
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
MYWORKD  DSECT                     *** MY WORK DSECT USING ANETWS2 ***          
SUMMARY  DS    CL1                                                              
MLTSDFLG DS    CL1                                                              
RESEED   DS    CL1                                                              
ASCRIBD  DS    CL1                                                              
CORRFLG  DS    CL1                                                              
RELO     DS    F                                                                
VNETWEEK DS    V                                                                
VDEMAND  DS    V                                                                
DEMOYR   DS    CL1                                                              
DEMOWK   DS    CL1                                                              
EBCDAT   DS    CL6                                                              
*                                                                               
DBLOCKWS DS    0CL270              *** DEDBLOCK DSECT FOLLOWS ***               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* NETINCLS                                                                      
* DDCOMFACSD                                                                    
* NEGETNUND                                                                     
       ++INCLUDE NETINCLN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF7D                                                       
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069NEMED07   03/19/20'                                      
         END                                                                    
