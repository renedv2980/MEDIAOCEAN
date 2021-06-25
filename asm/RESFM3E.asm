*          DATA SET RESFM3E    AT LEVEL 039 AS OF 05/01/02                      
*PHASE T8183EC,*                                                                
*INCLUDE LOADER                                                                 
*        TITLE 'T8183E - RESFM3E - KATZ WEEKLY STATION REPORT'                  
*                                                                               
**********************************************************************          
*                                                                    *          
*        RESFM3E (T8183E) --- KATZ WEEKLY STATION ACTIVITY REPORT    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
**********************************************************************          
                                                                                
T8183E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**183E**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         CLC   =C'WO',CONREC       IF OFFICE VERSION                            
         BE    *+8                                                              
         OI    GENSTAT2,NOREQDET   WE WANT REQUEST DETAILS                      
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***************************************************************                 
* VALIDATE KEY                                                                  
*                                                                               
VKEY     NTR1                                                                   
         CLC   =C'WO',CONREC       WEEKLY OFFICE VERSION?                       
         BNE   VK01                                                             
         MVI   OFFICVER,C'Y'       YES-OFFICE VERSION                           
         B     VKO1                                                             
VK01     XC    STATION,STATION                                                  
         MVI   ERROR,MISSING                                                    
         LA    R2,SOLSTATH                                                      
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    VK05                YES                                          
*                                                                               
         TM    WHEN,X'20'          NO           IF SOON?                        
         BNO   VK05                                                             
         CLI   5(R2),0                          MUST HAVE STATION               
         BE    VKERR                                                            
         MVI   ERROR,INVALID                                                    
         CLC   =C'ALL',8(R2)                    ALL NOT ALLOWED                 
         BE    VKERR                                                            
*                                                                               
VK05     CLC   =C'ALL',8(R2)                                                    
         BE    VK10                                                             
         GOTO1 VALISTA,DMCB                                                     
         MVC   STATION,WORK                                                     
         BAS   RE,CHKGRAPH                                                      
*                                                                               
VK10     LA    R2,SOLDATH          OVERRIDE  DATES                              
         XC    TODAY2,TODAY2                                                    
         XC    MONDAYDT,MONDAYDT                                                
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         MVI   ERROR,INVDATE                                                    
*                                                                               
         OC    ARFPBLK,ARFPBLK     IS IT RFP                                    
         BZ    VK22                                                             
         CLC   =Y(RE#RFPPS),9(R2)  ALREADY VALIDATED?                           
         BE    VK30                 YES                                         
         CLC   =C'PERSTEND',8(R2)   SYMBOLIC NAME?                              
         BNE   VK22                NO - CHECK IF DATE ENTERED                   
         BAS   RE,VALRFP           YES - LET RFP DO IT'S THING                  
         B     VK30                                                             
*                                                                               
VK22     BAS   RE,DASH                                                          
         LTR   R7,R7                                                            
         BNZ   VK25                                                             
                                                                                
* SINGLE DATE VALIDATION                                                        
         GOTO1 DATVAL,DMCB,(0,SOLDAT),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    VKERR                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE, COMPLETE DATES                   
         BNE   VK30                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(2,MONDAYDT)                                
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,TODAY2)                                
         B     VK26                                                             
                                                                                
* TWO DATE VALIDATION                                                           
VK25     BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),SOLDAT       VALIDATE 1ST MON/YR                          
         GOTO1 DATVAL,DMCB,(0,DUB),WORK                                         
         OC    DMCB(4),DMCB                                                     
         BZ    VKERR                                                            
         LA    R7,2(R7)            POINT R7 TO 2ND Y/M DATE                     
         LA    R3,SOLDAT                                                        
         AR    R3,R7                                                            
         GOTO1 DATVAL,DMCB,(0,0(R3)),WORK+6                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VKERR                                                            
         CLC   WORK(6),WORK+6                                                   
         BH    VKERR                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   VK30                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(2,MONDAYDT)                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,TODAY2)                                
                                                                                
*                                                                               
VK26     DS    0H                                                               
         B     VK30                                                             
*                                                                               
VK30     DS    0H                                                               
         B     VKX                                                              
*                                                                               
VKX      B     XIT                                                              
                                                                                
*                                                                               
* ROUTINE RETURNS LENGTH OF VALID FIELD BEFORE DASH IN R7 (LENGTH-1)            
* R2 POINTS TO FIELD HEADER                                                     
*                                                                               
DASH     SR    R4,R4                                                            
         IC    R4,5(R2)            LENGTH OF INPUT FIELD                        
         LA    R6,8(R2)            POINT R6 TO DATA                             
         SR    R7,R7                                                            
DASH2    CLI   0(R6),C'-'                                                       
         BE    DASH4                                                            
         LA    R6,1(R6)                                                         
         LA    R7,1(R7)            LENGTH OF VALID FIELD BEFORE DASH            
         BCT   R4,DASH2                                                         
         SR    R7,R7               SET R7 TO INDICATE NO DASH                   
         BR    RE                                                               
*                                                                               
DASH4    LTR   R7,R7               TEST DASH IN FIRST POSITION                  
         BZ    ERREND                                                           
         BR    RE                                                               
                                                                                
                                                                                
*                                                                               
ERREND   DS    0H                                                               
*                                                                               
VKERR    GOTO1 ERREX                                                            
         EJECT                                                                  
* OFFICE VERSION VALIDATION - USES OFFICE VERSION SCREEN T818AA                 
*                                                                               
VKO1     DS    0H                                                               
         XC    OFFICE,OFFICE                                                    
         XC    TEAM,TEAM                                                        
         XC    SLSPRSN,SLSPRSN                                                  
         XC    STATION,STATION                                                  
         MVI   ERROR,MISSING                                                    
                                                                                
*****************************************************************               
         LA    R2,SOOOFFH          OFFICE                                       
         TM    WHEN,X'20'                      ,, IF SOON?                      
         BNO   VKO5                                                             
         CLI   5(R2),0                         ,, MUST HAVE OFFICE              
         BE    VKERR                                                            
         MVI   ERROR,INVALID                                                    
         CLC   =C'ALL',8(R2)                   ,, ALL NOT ALLOWED               
         BE    VKERR                                                            
VKO5     CLI   5(R2),0             OFFICE INPUT?                                
         BNE   VKO7                YES                                          
         CLI   OFFLINE,C'Y'        NO                                           
         BE    VKO10                                                            
                                                                                
* TEST IF TERMINAL ALLOWED ACCESS TO ALL OFFICES                                
         CLI   TWAACCS,C'$'        .IF STATION                                  
         BE    VKO10               .ACCES TO ALL OFFICES                        
         TM    12(RA),X'80'        TEST IF TERMINAL ALLOWED ACCESS              
         BO    VKO10                                                            
         CLI   1(RA),C'*'          TEST FOR DDS TERMINAL                        
         BE    VKO10                                                            
         MVI   ERROR,INVALID                                                    
         B     VKERR                                                            
                                                                                
*                                                                               
VKO7     GOTO1 VALIOFF,DMCB          VALIDATE OFFICE                            
         MVC   OFFICE,WORK           SAVE OFFICE CODE                           
         MVC   OFFICNAM,WORK+2       SAVE OFFICE NAME                           
         MVC   SOOOFFN,WORK+2                                                   
         OI    SOOOFFNH+6,X'80'    TRANSMIT NAME                                
                                                                                
                                                                                
*******************************************************************             
VKO10    LA    R2,SOOTEMH          TEAM                                         
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    VKO20                                                            
         CLC   =C'ALL',8(R2)                                                    
         BE    VKO20                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,5                                                            
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),SOOTEM                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VKERR                                                            
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RTEMREC,R1                                                       
         MVC   TEAM,RTEMKTEM       DIV/TEAM                                     
         MVC   TEAMNM,RTEMDVNM     DIV/TEAM NAME                                
         MVC   SOOTEMN,TEAMNM                                                   
         OI    SOOTEMNH+6,X'80'   TRANSMIT FIELD                                
         DROP  R1                                                               
                                                                                
*******************************************************************             
         LA    R2,SOOSALH          SALESPERSON                                  
         CLI   5(R2),0                                                          
         BE    VKO20                                                            
*                                                                               
         MVI   ERROR,INVALID                                                    
         MVI   KEY,6                                                            
         XC    KEY+1(21),KEY+1                                                  
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),SOOSAL                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VKERR                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RSALREC,R1                                                       
         MVC   SOOSALN,RSALNAME    TO SCREEN                                    
         MVC   SLSNAME,RSALNAME    TO SAVE AREA                                 
         MVC   SOOSALNH+6,X'80'                                                 
                                                                                
         CLI   TEAM,0              IF FILTERING ON TEAM                         
         BE    *+14                                                             
         CLC   TEAM,RSALTEAM       CHECK SALESTEAM VS FILTER                    
         BNE   VKERR                                                            
                                                                                
*****************************************************************               
VKO20    LA    R2,SOOSTATH         STATION                                      
         CLI   5(R2),0                                                          
         BE    VKO30                                                            
         CLC   =C'ALL',8(R2)                                                    
         BE    VKO30                                                            
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
         BAS   RE,CHKGRAPH                                                      
                                                                                
*******************************************************************             
* REPORT'S DATES ARE FROM PREVIOUS MONDAY TO RUN DATE OF REPORT                 
*                                                                               
VKO30    LA    R2,SOODATH          OVERRIDE DATES ?                             
         XC    TODAY2,TODAY2                                                    
         XC    MONDAYDT,MONDAYDT                                                
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         MVI   ERROR,INVDATE                                                    
*                                                                               
         OC    ARFPBLK,ARFPBLK     IS IT RFP                                    
         BZ    VKO32                                                            
         CLC   =Y(RE#RFPPS),9(R2)  ALREADY VALIDATED?                           
         BE    VKO40               YES                                          
         CLC   =C'PERSTEND',8(R2)  SYMBOLIC NAME?                               
         BNE   VKO32               NO - CHECK IF DATE ENTERED                   
         BAS   RE,VALRFP           YES - LET RFP DO IT'S THING                  
         B     VKO40                                                            
*                                                                               
VKO32    BAS   RE,DASH                                                          
         LTR   R7,R7                                                            
         BNZ   VKO35                                                            
                                                                                
* SINGLE DATE VALIDATION                                                        
         GOTO1 DATVAL,DMCB,(0,SOODAT),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         CLI   OFFLINE,C'Y'        IF OFFLINE, COMPLETE DATES                   
         BNE   VKO40                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,MONDAYDT)                                
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,TODAY2)                                
         B     VKO36                                                            
                                                                                
* TWO DATE VALIDATION                                                           
VKO35    BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),SOODAT       VALIDATE 1ST MON/YR                          
         GOTO1 DATVAL,DMCB,(0,DUB),WORK                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         LA    R7,2(R7)            POINT R7 TO 2ND Y/M DATE                     
         LA    R3,SOODAT                                                        
         AR    R3,R7                                                            
         GOTO1 DATVAL,DMCB,(0,0(R3)),WORK+6                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         CLC   WORK(6),WORK+6                                                   
         BH    ERREND                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   VKO40                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,MONDAYDT)                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,TODAY2)                                
                                                                                
*                                                                               
VKO36    DS    0H                                                               
         B     VKO40                                                            
*                                                                               
VKO40    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* RFP VALIDATION                                                                
VALRFP   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
                                                                                
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING QRFPD,R3                                                         
         MVI   QRFPMODE,QRFPSYMB   SYMBOLIC NAME VALIDATION                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)   PASS SYMBOLIC NAME                           
         OC    QRFPWORK,SPACES                                                  
         GOTO1 RFP,DMCB,(R3)                                                    
         OC    QRFPWORK,QRFPWORK   ERROR                                        
         BZ    ERREND                                                           
         MVC   8(L'QRFPESC,R2),QRFPWORK                                         
         MVI   5(R2),8             SET LENGTH OF EXPLODED DATA                  
         MVI   11(R2),8            PASS LENGTH OF EXPLODED DATA                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
VALRFPX  XIT1                                                                   
         DROP  R3                                                               
                                                                                
CHKGRAPH DS    0H                                                               
* CHECK IF STATION IS FAX STATION                                               
         L     R1,AIO                                                           
         USING RSTAREC,R1                                                       
         OC    RSTAEND,RSTAEND     IS STATION STILL GRAPHNET?                   
         BNZ   GRAPHERR                                                         
         CLI   RSTATRAF,C'G'        AND MUST BE A/G TRAFFIC                     
         BER   RE                                                               
         CLI   RSTATRAF,C'A'                                                    
         BER   RE                                                               
         B     GRAPHERR                                                         
         DROP  R1                                                               
*                                                                               
GRAPHERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(25),=C'ERROR - NOT A FAX STATION'                        
         GOTO1 ERREX2                                                           
                                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
RECORDS  DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE REGENTEM                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE DDWIDED                                                        
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM3FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMAAD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
*                                                                               
*               WORK AREA                                                       
*** ---> AREA BELOW TO BE KEPT IN SYNC WITH T8183D - PRINT MODULE               
         DS    0F                                                               
MYWORK   DS    0CL1000                                                          
OFFICVER DS    CL1                 OFFICE VERSION OF REPORT                     
STATION  DS    CL5                 STATION FILTER                               
OFFICE   DS    CL2                 OFFICE FILTER                                
OFFICNAM DS    CL20                OFFICE NAME                                  
TEAM     DS    CL2                 TEAM FILTER                                  
TEAMNM   DS    CL20                DIV/TEAM NAME                                
SLSPRSN  DS    CL3                 INITIALS                                     
SLSNAME  DS    CL20                SALESPERSON NAME                             
*                                                                               
ATSAROFF DS    F                                                                
ACTSTR   DS    CL3                 YMD (OVERRIDES PREV MON-RUNDAT)              
ACTEND   DS    CL3                 YMD  (OVERRIDES PREV MON-RUNDAT)             
YMDST    DS    CL3                 START DATE YMD                               
YMDND    DS    CL3                 END DATE YMD                                 
TODAY2   DS    CL2                 TODAY'S DATE COMPRESSED                      
FAXNUMBR DS    CL13                                                             
PREVIOUS DS    CL40                                                             
WORK2    DS    CL200                                                            
STAHEAD  DS    CL5                                                              
REPHEAD  DS    CL2                                                              
OPENFAX  DS    CL1                                                              
*                                                                               
MONDAYDT DS    CL3                 PREVIOUS MONDAY'S DATE                       
MYWORKND EQU   *                                                                
*                                                                               
* RFP INCLUDES                                                                  
       ++INCLUDE GEGENRFPD                                                      
       ++INCLUDE REDDEQUS                                                       
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039RESFM3E   05/01/02'                                      
         END                                                                    
