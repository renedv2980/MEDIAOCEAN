*          DATA SET CTSFM51    AT LEVEL 028 AS OF 05/01/02                      
*PHASE TA0A51A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM51 -- STAFF RECORDS   MAINTENANCE/LIST/REPORT   *         
*                                                                     *         
*  COMMENTS:     MAINTAINS STAFF RECORDS ON GENDIR/GENFIL             *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMA1 (MAINTENANCE)                        *         
*                        CTSFMB1 (LIST)                               *         
*                        CTSFMC1 (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED STAFF RECORDS, LIST, OR REPORT.              *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A51 - STAFF RECORD MAINT/LIST/REPORT'                        
TA0A51   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A51**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
*                                                                               
VK       XC    STAFFID,STAFFID     STAFF ID NAME                                
         LA    R2,STFPRIDH                                                      
         CLI   ACTEQU,ACTLIST      ON-SCREEN LIST ACTION?                       
         BE    VK10                                                             
         CLI   ACTEQU,ACTREP       REPORT ACTION?                               
         BE    VK20                                                             
         GOTO1 ANY                                                              
         CLI   STFPRIDH+5,8        ENTERED FULL ID?                             
         BNE   *+14                                                             
         MVC   STAFFID,WORK                                                     
         B     VK20                                                             
         CLI   STFPRIDH+5,5        ENTERED ABBR. ID?                            
         BNE   *+20                                                             
         MVC   STAFFID(4),WORK                                                  
         MVC   STAFFID+4(4),=C'DDNY'                                            
         B     VK20                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         BE    VSFMERR                                                          
         B     VK20                                                             
*                                                                               
VK10     CLI   STFPRIDH+5,0        ENTERED ANYTHING?                            
         BE    VK20                                                             
         GOTO1 ANY                 TO PUT INTO WORK                             
         CLI   SFLPRIDH+5,3         LENGTH OF THREE                             
         BNE   *+24                                                             
         CLC   =C'ALL     ',WORK   'ALL' STAFF MEMEBERS?                        
         BNE   *+14                                                             
         XC    STAFFID,STAFFID                                                  
         B     VK20                                                             
         MVC   STAFFID,WORK        FOR LIST START AT                            
         B     VK20                                                             
*                                                                               
VK20     LA    R4,KEY              BUILD KEY                                    
         USING ASTFKEYD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   ASTFSYS,ASTFSYSQ    SYSTEM                                       
         MVI   ASTFTYP,ASTFTYPQ    RECORD TYPE                                  
         MVC   ASTFPRID,STAFFID    SYSTEM/PROGRAM                               
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       L     R4,AIO              BUILD RECORD                                 
         USING ASTFKEY,R4                                                       
         MVC   STAFFID,ASTFPRID    STAFF ID                                     
*                                                                               
         MVI   ELCODE,STNAMELQ     NAME ELEMENT                                 
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING STNAMD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   STNAMEL,STNAMELQ                                                 
         MVI   STNAMLN,STNAMLNQ                                                 
         LA    R2,STFNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   STNAME,WORK         ACTUAL NAME                                  
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,PHNUMELQ     PHONE NUMBER ELEMENT                         
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING PHNUMD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   PHNUMEL,PHNUMELQ                                                 
         MVI   PHNUMLN,PHNUMLNQ                                                 
         LA    R2,STFPOFFH         OFFICE EXTENTION                             
         GOTO1 ANY                                                              
         MVC   PHNUMOFF,WORK                                                    
         LA    R2,STFPHMEH         HOME PHONE NUMBER                            
         GOTO1 ANY                                                              
         MVC   PHNUMHME,STFPHME                                                 
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR20     B     DR                  DISPLAY THE RECORD                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R0,STFMTAGH         LAST FIELD ON SCREEN                         
         LA    R2,STFNAMEH         FIRST DATA FIELD HEADER                      
*                                                                               
DR10     ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         SH    R1,=H'17'           MINUS HEADER, EXTEN, AND 1 FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DR20     ZIC   R1,0(R2)            RESTORE LENGTH                               
         AR    R2,R1               NEXT SCREEN FIELD                            
         CR    R2,R0               END OF SCREEN?                               
         BE    *+16                                                             
         TM    1(R2),X'20'         NO -- FIELD IS PROTECTED?                    
         BZ    DR10                NO -- CLEAR IT                               
         B     DR20                YES -- BUMP TO NEXT FIELD                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STNAMELQ     NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING STNAMD,R6                                                        
         MVC   STFNAME,STNAME                                                   
         OI    STFNAMEH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PHNUMELQ     PHONE NUMBERS                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PHNUMD,R6                                                        
         MVC   STFPOFF,PHNUMOFF                                                 
         OI    STFPOFFH+6,X'80'                                                 
         MVC   STFPHME,PHNUMHME                                                 
         OI    STFPHMEH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              SELECTED RECORD                              
         USING ASTFKEYD,R4                                                      
*                                                                               
         MVC   STFPRID,ASTFPRID    STAFF ID NAME                                
         OI    STFPRIDH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ON-SCREEN LIST                                                                
*                                                                               
LR       LA    R4,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
         MVI   ASTFSYS,ASTFSYSQ    SYSTEM                                       
         MVI   ASTFTYP,ASTFTYPQ    RECORD TYPE                                  
         MVC   ASTFPRID,STAFFID    STAFF ID NAME                                
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(2),SAVEKEY      SAME RECORD TYPE?                            
         BNE   LRX                 NO MORE TO LIST                              
         GOTO1 GETREC              GET THE ASTAFF RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSTSTFID,ASTFPRID   PUT STAFF ID NAME IN LIST LINE               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STNAMELQ     NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING STNAMD,R6                                                        
         MVC   LSTNAME,STNAME      PUT NAME IN LIST LINE                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PHNUMELQ     PHONE NUMBER ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PHNUMD,R6                                                        
         MVC   LSTPOFF,PHNUMOFF    PUT NUMBERS IN LIST LINE                     
         MVC   LSTPHME,PHNUMHME                                                 
*                                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* OFFLINE PRINT                                                                 
*                                                                               
PR       LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    PR10                NO                                           
         USING BOXD,RF                                                          
         DROP  RF                                                               
*                                                                               
PR10     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   ASTFSYS,ASTFSYSQ    SYSTEM                                       
         MVI   ASTFTYP,ASTFTYPQ    RECORD TYPE                                  
         XC    STAFFID,STAFFID     PRINT ALL STAFF IDS                          
         MVC   ASTFPRID,STAFFID                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR30     CLC   KEY(2),SAVEKEY      ASTAFF RECORD?                               
         BNE   PRX                 NO MORE TO REPORT                            
*                                                                               
         GOTO1 GETREC              GET THE STAFF RECORD                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVC   PRTSTAFF,ASTFPRID   PUT STAFF ID NAME IN PRINT LINE              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STNAMELQ     NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING STNAMD,R6                                                        
         MVC   PRTNAME,STNAME      PUT NAME IN PRINT LINE                       
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PHNUMELQ     PHONE NUMBERS ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PHNUMD,R6                                                        
         MVC   PRTEXT,PHNUMOFF                                                  
         MVC   PRTHOME,PHNUMHME                                                 
         DROP  R6                                                               
*                                                                               
PR40     GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
         B     PR20                NEXT RECORD                                  
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H3+55(19),=C'STAFF RECORD REPORT'                                
         MVI   H4+55,X'BF'         UNDERLINE CHARACTER                          
         MVC   H4+56(18),H4+55                                                  
*                                                                               
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
*                                                                               
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+34,C'C'                                                  
         MVI   BOXCOLS+39,C'C'                                                  
         MVI   BOXCOLS+52,C'R'                                                  
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
*                                                                               
HDHOOKX  B     XIT                                                              
         SPACE 3                                                                
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H7,2,C'ID       NAME'                                            
         SSPEC H7,36,C'EXT. HOME PHONE #'                                       
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC1D                                                       
         EJECT                                                                  
       ++INCLUDE CTGENASTAF                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
STAFFID  DS    CL8                 STAFF ID NAME                                
SAVEKEY  DS    XL32                GENFILE KEY                                  
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSTFID DS    CL8                 STAFF ID NAME                                
         DS    CL2                                                              
LSTNAME  DS    CL24                NAME                                         
         DS    CL2                                                              
LSTPOFF  DS    CL4                 OFFICE EXTENTION                             
         DS    CL4                                                              
LSTPHME  DS    CL12                HOME PHONE                                   
         SPACE 5                                                                
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                   LEFT BOX MARGIN                              
PRTSTAFF DS    CL8                 STAFF ID                                     
         DS    C                                                                
PRTNAME  DS    CL24                NAME                                         
         DS    C                                                                
PRTEXT   DS    CL4                 EXTENTION                                    
         DS    C                                                                
PRTHOME  DS    CL12                HOME PHONE                                   
         DS    C                   RIGHT BOX MARGIN                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028CTSFM51   05/01/02'                                      
         END                                                                    
