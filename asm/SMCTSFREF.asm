*          DATA SET SMCTSFREF  AT LEVEL 098 AS OF 05/01/02                      
*          DATA SET CTSFM65    AT LEVEL 195 AS OF 05/10/94                      
*          DATA SET CTSFM65    AT LEVEL 171 AS OF 05/10/94                      
*PHASE TA0A65                                                                   
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A65 - TEAM RECORD MAINTENANCE/LIST                                 
*                                                                     *         
*  COMMENTS: MAINTAINS TEAM RECORDS.                                            
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMA5 (TA0AA5) -- MAINTENANCE                    *         
*                  CTSFMA6 (TA0AA6) -- LIST                           *         
*                  CTSFMB5 (TA0AB5) -- REPORT                         *         
*                                                                     *         
*  OUTPUTS: UPDATED TEAM RECORDS                                                
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                                     
*          R8 - WORK                                                  *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A65 TEAM RECORD MAINTENANCE/LIST'                            
TA0A65   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A65*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         XC    ACURSOR,ACURSOR                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,XRECPUT        RECORDED CHANGED                             
         BE    DR                                                               
         CLI   MODE,XRECADD        RECORDED ADDED                               
         BE    DR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE KEY ROUTINE                                                          
**********************************************************************          
*                                                                               
VK       XC    KEY,KEY             CLEAR KEY                                    
         LA    R4,KEY                                                           
         XC    SAVESPR,SAVESPR     CLEAR SAVES                                  
         XC    SAVETEAM,SAVETEAM                                                
         XC    SAVEYEAR,SAVEYEAR                                                
*                                                                               
VKSPR    LA    R2,SFMSPRH          SPORT CODE                                   
         XC    SAVESPR,SAVESPR                                                  
         CLI   5(R2),0             ANY KEY?                                     
         BNE   VKSPRGD                                                          
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VKSPR01                                                          
         CLI   ACTNUM,ACTREP       OPTIONAL FOR REPORT                          
         BNE   MISSERR                                                          
VKSPR01  MVC   SFMSINF,BLANKS      BLANK OUT INFO LINE IF NO SPORT              
         OI    SFMSINFH+6,X'80'                                                 
         B     VKYEAR                                                           
VKSPRGD  MVC   SAVESPR,SFMSPR      SAVE IT                                      
         GOTO1 VLDSPR,DMCB,SFMSINF,L'SFMSINF,SFMSPR,1                           
         OI    SFMSINFH+6,X'80'                                                 
*                                                                               
VKTEAM   CLI   ACTNUM,ACTLIST      NO TEAM FILTER FOR LIST                      
         BE    VKYEAR                                                           
         CLI   ACTNUM,ACTREP       NO TEAM FOR REPORT                           
         BE    VKYEAR                                                           
         LA    R2,SFMTEAMH         TEAM CODE                                    
         XC    SAVETEAM,SAVETEAM                                                
         CLI   5(R2),0             ANY KEY?                                     
         BNE   VKTEAMGD                                                         
         B     MISSERR                                                          
*                                                                               
VKTEAMGD MVC   SAVETEAM,SFMTEAM    SAVE IT                                      
         GOTO1 VLDTEAM,DMCB,SFMTINF,SFMSPR,SFMTEAM,1,L'SFMTINF                  
         OI    SFMTINFH+6,X'80'                                                 
*                                                                               
VKYEAR   LA    R2,SFMYEARH         YEAR CODE                                    
         BAS   RE,NUMERIC          NUMERIC?                                     
*                                                                               
VKYEAR01 CLI   5(R2),0             ENTERED ANYTHING?                            
         BNE   VKYEARGD                                                         
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VKPP                                                             
         CLI   ACTNUM,ACTREP       OPTIONALFOR REPORT                           
         BE    VKPP                                                             
         B     MISSERR                                                          
VKYEARGD ICM   R1,3,SFMYEAR                                                     
         LCR   R1,R1                                                            
         STCM  R1,3,SAVEYEAR                                                    
*                                                                               
VKPP     CLI   ACTNUM,ACTREP                                                    
         BNE   VKX                                                              
         LA    R2,SFRPPH                                                        
         CLI   SFRPP,C'Y'                                                       
         BE    VKX                                                              
         CLI   SFRPP,C'N'                                                       
         BE    VKX                                                              
         MVC   CONHEAD,=CL60'PLEASE ANSWER ''Y'' OR ''N'''                      
         GOTO1 ERREX2                                                           
*                                                                               
VKX      DS    0H                                                               
         XC    KEY,KEY                                                          
         USING TMKEY,R4                                                         
         MVI   TMKTYPE,TMKTYPEQ                                                 
         MVC   TMKSPR,SAVESPR                                                   
         MVC   TMKTEAM,SAVETEAM                                                 
         MVC   TMKYEAR,SAVEYEAR                                                 
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE TEAM CODE AND DISPLAY INFO                                           
***********************************************************************         
* P1 = A(DEST)                                                                  
* P2 = A(SPORT CODE)                                                            
* P3 = A(TEAM CODE)                                                             
* IF P4 <> 0, GOTO ERREX2 IF TEAM CODE IS INVALID                               
* P5 = LEN OF DEST                                                              
VLDTEAM  NTR1                                                                   
         LM    R3,R7,0(R1)                                                      
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY2,KEY                                                     
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING TMDKEY,R8                                                        
         MVI   TMDTYPE,TMDTYPEQ                                                 
         MVC   TMDSCODE,0(R4)                                                   
         MVC   TMDTCODE,0(R5)                                                   
         MVC   SAVEKEY3,KEY                                                     
         GOTO1 HIGH                                                             
         CLC   SAVEKEY3,KEY                                                     
         BE    GOTEAM                                                           
         LTR   R6,R6               SEE IF WE SHOULD BAIL IF INVALID             
         BNZ   INVTEAM                                                          
         MVC   0(16,R3),=C'** INVALID TC **'                                    
         B     VLDTEAMX                                                         
*                                                                               
* DISPLAY FULL TEAM NAME                                                        
GOTEAM   GOTO1 GETREC                                                           
         L     R8,AIO                                                           
         LA    R6,TMDFSTEL     ADDR OF FIRST ELEMENT                            
         DROP  R8                                                               
         MVI   ELCODE,TMDNAMEQ                                                  
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TMDNAMD,R6                                                       
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TMDNAMNM                                                 
         DROP  R6                                                               
*                                                                               
VLDTEAMX MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY2                                                     
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE SPORT CODE AND DISPLAY INFO                                          
***********************************************************************         
* P1 = A(DEST)                                                                  
* P2 = LEN OF DESTINATION FIELD                                                 
* P3 = A(SPORT CODE)                                                            
* IF P4 <> 0, THEN IF SPORT CODE IS INVALID GOES TO ERREX2                      
VLDSPR   NTR1                                                                   
         LM    R3,R6,0(R1)                                                      
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY2,KEY                                                     
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING SPRKEYD,R8                                                       
         MVI   SPRTYPE,SPRTYPEQ                                                 
         MVC   SPRSPRCD,0(R5)       SPORT CODE                                  
         MVC   SAVEKEY3,KEY                                                     
         GOTO1 HIGH                                                             
         CLC   SAVEKEY3,KEY         KEY VALID?                                  
         BE    GOSPR                                                            
         LTR   R6,R6               SEE IF WE SHOULD BAIL IF INVALID             
         BNZ   INVSPR                                                           
         MVC   0(24,R3),=C'** INVALID SPORT CODE **'                            
         B     VLDSPRX                                                          
*                                                                               
*                                                                               
* DISPLAY FULL SPORT NAME                                                       
GOSPR    GOTO1 GETREC                                                           
         L     R8,AIO                                                           
         LA    R6,SPRFSTEL     ADDR OF FIRST ELEMENT                            
         DROP  R8                                                               
         MVI   ELCODE,SPRMNEQ                                                   
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SPRMNED,R6                                                       
         MVC   0(L'SPRMNSPR,R3),SPRMNSPR                                        
         LA    R3,0(R3,R4)                                                      
         SH    R3,=H'8'                                                         
         MVI   0(R3),C'('                                                       
         GOTO1 OUTSN,DMCB,1(R3),SPRMNSN                                         
         L     R3,0(R1)                                                         
         MVI   0(R3),C')'                                                       
*                                                                               
VLDSPRX  MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY2                                                     
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE RECORD ROUTINE                                                       
**********************************************************************          
*                                                                               
VR       DS    0H                                                               
         MVI   ELCODE,TMMNEQ       MAIN ELEMENT                                 
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING TMMNED,R6                                                        
         MVI   TMMNEL,TMMNEQ       ELEMENT CODE                                 
         MVI   TMMNLN,TMMNLQ       ELEMENT LENGTH                               
*                                                                               
         LA    R2,SFMWINH                                                       
         BAS   RE,NUMERIC          NUMERIC?                                     
         MVC   TMMNWIN,8(R2)       MOVE IN WINS                                 
         LA    R2,SFMLOSH                                                       
         BAS   RE,NUMERIC          NUMERIC?                                     
         MVC   TMMNLOS,8(R2)               LOSSES                               
         LA    R2,SFMTIEH                                                       
         BAS   RE,NUMERIC          NUMERIC?                                     
         MVC   TMMNTIE,8(R2)               TIES                                 
*                                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
VRPLREL  MVI   ELCODE,TMPLREQ      PLAYER ELEMENT                               
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING TMPLRED,R6                                                       
         LA    R2,SFMIDH                                                        
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VRPLRLP  LA    R0,SFMENDH          LAST PLAYER CODE ON SCREEN                   
         CR    R2,R0               END OF SCREEN?                               
         BH    VRX                                                              
         CLI   5(R2),0             ANYTHING ENTERED?                            
         BNE   VRPLRGO             IF NOT, SKIP THIS LINE                       
         ZIC   R0,0(R2)            BUMP FIELD POINTER FOUR TIMES                
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VRPLRLP                                                          
*                                                                               
VRPLRGO  LR    R3,R2               SAVE POINTER TO FIRST FIELD OF LINE          
         XC    ELEM,ELEM                                                        
         MVI   TMPLREL,TMPLREQ     ELEMENT CODE                                 
         MVI   TMPLRLN,TMPLRLQ     ELEMENT LENGTH                               
*                                                                               
* VALIDATE ID                                                                   
         BAS   RE,VLDID                                                         
         MVC   TMPLRID,8(R2)       PROFS ID                                     
         ZIC   R0,0(R2)            BUMP FIELD POINTER TWICE                     
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
* VALIDATE NUM                                                                  
         BAS   RE,VLDNUM                                                        
         MVC   TMPLRNUM,8(R2)      JERSEY NUMBER                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP FIELD POINTER                           
*                                                                               
* VALIDATE POS                                                                  
         BAS   RE,VLDPOS                                                        
         MVC   TMPLRPOS,8(R2)      POSITION CODE                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP FIELD POINTER                           
         DROP  R6                                                               
*                                                                               
* VALIDATE NO REPS OF ID OR NUMBER                                              
         BAS   RE,VLDNOREP                                                      
*                                                                               
* EVERYTHING VALID, ADD ELEMENT AND LOOP                                        
         MVI   ELCODE,TMPLREQ      PLAYER ELEMENT                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VRPLRLP                                                          
*                                                                               
VRX      MVC   KEY,SAVEKEY                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    VRXX                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                RESTORE BUFFERS FOR DMGR                     
         GOTO1 GETREC                                                           
VRXX     MVC   AIO,AIO1                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE NO REPETITIONS IN ID, NUM                                            
***********************************************************************         
* R2 POINTS TO ID FIELD, R6 POINTS TO CURRENT ELEMENT                           
*                                                                               
VLDNOREP NTR1                                                                   
         LR    R5,R6               COPY ADDRESS OF CURRENT ELEM                 
         L     R6,AIO                                                           
         LA    R6,TMFSTEL-TMKEY(R6)   ADDR OF FIRST ELEM                        
         MVI   ELCODE,TMPLREQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   VLDNOX                                                           
*                                                                               
         USING TMPLRED,R6                                                       
VRCHKLP  CLC   TMPLRID,TMPLRID-TMPLRED(R5)                                      
         BNE   VRCHKPOS                                                         
         LR    R2,R3               POSITION CURSOR ON FIELD                     
         MVC   CONHEAD,=CL60'ERROR: DUPLICATE PROFS ID'                         
         GOTO1 ERREX2                                                           
*                                                                               
VRCHKPOS CLC   TMPLRNUM,TMPLRNUM-TMPLRED(R5)                                    
         DROP  R6                                                               
         BNE   VRNXTCHK                                                         
         LR    R2,R3               POSITION CURSOR ON FIELD                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   CONHEAD,=CL60'ERROR: DUPLICATE NUMBER'                           
         GOTO1 ERREX2                                                           
*                                                                               
VRNXTCHK BAS   RE,NEXTEL                                                        
         BE    VRCHKLP                                                          
*                                                                               
VLDNOX   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE PROFS ID IS VALID                                                    
**********************************************************************          
*                                                                               
VLDID    NTR1                                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PLAKEY,R4                                                        
         MVI   PLAKTYPE,PLAKTYPQ                                                
         MVC   PLAKID,8(R2)                                                     
         MVC   SAVEKEY2,KEY                                                     
         GOTO1 HIGH                                                             
         CLC   SAVEKEY2,KEY                                                     
         BE    CKID                                                             
         MVC   CONHEAD,=CL60'PROFS ID NOT VALID'                                
         GOTO1 ERREX2                                                           
*                                                                               
* CHECK PERSON LIKES SPORT                                                      
CKID     MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R6,PLAFSTEL                                                      
         DROP  R4                                                               
         MVI   ELCODE,PLASPRLQ     GET FAVORITE SPORT ELEMS                     
         BAS   RE,FIRSTEL                                                       
         BNE   IDERR                                                            
*                                                                               
*  COMPARE WITH ALL FAVORITE SPORTS                                             
         USING PLASPORD,R6                                                      
VRIDVLP  CLC   PLASFAV,SFMSPR                                                   
         DROP  R6                                                               
         BE    VLDIDX                                                           
         BAS   RE,NEXTEL                                                        
         BE    VRIDVLP                                                          
*                                                                               
IDERR    MVC   CONHEAD,=CL60'ERROR: PERSON DOES NOT PLAY THIS SPORT'            
         GOTO1 ERREX2                                                           
*                                                                               
VLDIDX   MVC   AIO,AIO1                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE JERSEY NUMBER NOT RETIRED                                            
**********************************************************************          
*                                                                               
VLDNUM   NTR1                                                                   
         GOTO1 NUMERIC                                                          
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TMDKEY,R4                                                        
         MVI   TMDTYPE,TMDTYPEQ                                                 
         MVC   TMDSCODE,SFMSPR                                                  
         MVC   TMDTCODE,SFMTEAM                                                 
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R6,TMDFSTEL         A(FIRST ELEMENT)                             
         DROP  R4                                                               
         MVI   ELCODE,TMDRETEQ     GET RETIRED ELEMENT NUMS                     
         BAS   RE,FIRSTEL                                                       
         BNE   VLDNUMX                                                          
*                                                                               
*  COMPARE WITH ALL RETIRED NUMS                                                
         USING TMDRETD,R6                                                       
VR#VLP   CLC   TMDRETNO,8(R2)                                                   
         DROP  R6                                                               
         BNE   VR#NEXT                                                          
         MVC   CONHEAD,=CL60'ERROR: NUMBER RETIRED.  CHOOSE ANOTHER'            
         GOTO1 ERREX2                                                           
VR#NEXT  BAS   RE,NEXTEL                                                        
         BE    VR#VLP                                                           
*                                                                               
VLDNUMX  MVC   AIO,AIO1                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE POSITION CODE EXISTS FOR SPORT                                       
**********************************************************************          
*                                                                               
VLDPOS   NTR1                                                                   
         GOTO1 ANY                                                              
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING SPRKEYD,R4                                                       
         MVI   SPRTYPE,SPRTYPEQ                                                 
         MVC   SPRSPRCD,SFMSPR      SPORT CODE                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R6,SPRFSTEL         A(FIRST ELEMENT)                             
         DROP  R4                                                               
         MVI   ELCODE,SPRPOSEQ     GET POSITIONS                                
         BAS   RE,FIRSTEL                                                       
         BNE   POSERR                                                           
*                                                                               
*  COMPARE WITH ALL POSITIONS                                                   
         USING SPRPOSED,R6                                                      
VRPOSVLP CLC   SPRPOSPC,8(R2)                                                   
         DROP  R6                                                               
         BE    VLDPOSX                                                          
         BAS   RE,NEXTEL                                                        
         BE    VRPOSVLP                                                         
*                                                                               
POSERR   MVC   CONHEAD,=CL60'ERROR: INVALID POSITION'                           
         GOTO1 ERREX2                                                           
*                                                                               
VLDPOSX  MVC   AIO,AIO1                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* DISPLAY RECORD                                                                
**********************************************************************          
*                                                                               
DR       L     R4,AIO                                                           
         USING TMKEY,R4                                                         
*                                                                               
         LA    R6,TMFSTEL          A(FIRST ELEMENT)                             
         USING TMMNEL,R6                                                        
         LA    R2,SFMWINH          TEAM NAME FIELD                              
         MVI   ELCODE,TMMNEQ       MAIN ELEMENT CODE                            
         BAS   RE,FIRSTEL          MAIN ELEMENT IS THERE?                       
         BE    *+6                                                              
         DC    H'0'                REQUIRED ELEMENT!!!                          
         MVC   SFMWIN,TMMNWIN      PUT WINS IN FIELD                            
         MVC   SFMLOS,TMMNLOS          LOSSES                                   
         MVC   SFMTIE,TMMNTIE          TIES                                     
         OI    SFMWINH+6,X'80'     XMIT                                         
         OI    SFMLOSH+6,X'80'     XMIT                                         
         OI    SFMTIEH+6,X'80'     XMIT                                         
*                                                                               
         LA    R2,SFMIDH           FIRST ID FIELD                               
         SR    R3,R3               R3=0, AS LONG AS GETEL RETURNS ELEM          
         LA    R6,TMFSTEL          A(FIRST ELEMENT)                             
         USING TMPLREL,R6                                                       
         MVI   ELCODE,TMPLREQ      PLAYER ELEMENT CODE                          
         BAS   RE,FIRSTEL          ANY PLR FIELDS?                              
         BE    DRPLRLP                                                          
         LA    R3,1                FLAG NO ELEMENT                              
*                                                                               
* PLAYER LOOP                                                                   
DRPLRLP  LTR   R3,R3               IF VALID ELEMENT . . .                       
         BZ    DRPLRID                                                          
*                                                                               
* NO MORE VALID ELEMENTS, OUTPUT BLANK FIELD                                    
         MVC   8(L'SFMID,R2),BLANKS                                             
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP FIELD POINTER                           
         MVC   8(L'SFMNAM,R2),BLANKS                                            
         B     DRPLRNUM                                                         
*                                                                               
* VALID ELEMENT, OUTPUT ID AND INFO                                             
DRPLRID  MVC   8(L'SFMID,R2),TMPLRID   ... ID TO FIELD                          
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP FIELD POINTER                           
         GOTO1 DISPNAM,DMCB,8(R2),TMPLRID                                       
*                                                                               
DRPLRNUM OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            BUMP POINTER                                 
         AR    R2,R0                                                            
*                                                                               
         MVC   8(L'SFMNUM,R2),BLANKS                                            
         LTR   R3,R3               IF VALID ELEMENT . . .                       
         BNZ   *+10                                                             
         MVC   8(L'SFMNUM,R2),TMPLRNUM  ... NUMBER TO FIELD                     
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               BUMP FIELD POINTER                           
*                                                                               
         MVC   8(L'SFMPOS,R2),BLANKS                                            
         LTR   R3,R3               IF VALID ELEMENT . . .                       
         BNZ   *+10                                                             
         MVC   8(L'SFMPOS,R2),TMPLRPOS  ... POSITION TO FIELD                   
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               BUMP FIELD POINTER                           
*                                                                               
         MVI   ELCODE,TMPLREQ      PLAYER ELEMENT CODE                          
         BAS   RE,NEXTEL                                                        
         BE    DRPLRNXT                                                         
         LA    R3,1                IF NOT VALID ELEM, FLAG IT                   
DRPLRNXT LA    R1,SFMENDH                                                       
         CR    R2,R1               END OF SCREEN?                               
         BNH   DRPLRLP                                                          
*                                                                               
DRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* DISPLAY KEY                                                                   
**********************************************************************          
DK       L     R4,AIO              RECORD SELECTED                              
         MVC   SAVEKEY,KEY                                                      
         USING TMKEY,R4                                                         
         ICM   R1,3,TMKYEAR                                                     
         LCR   R1,R1                                                            
         STCM  R1,3,SFMYEAR        DISPLAY YEAR CODE                            
         MVC   SFMSPR,TMKSPR               SPORT CODE                           
         MVC   SFMTEAM,TMKTEAM             TEAM CODE                            
         GOTO1 VLDSPR,DMCB,SFMSINF,L'SFMSINF,SFMSPR,0                           
         GOTO1 VLDTEAM,DMCB,SFMTINF,SFMSPR,SFMTEAM,0,L'SFMTINF                  
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ONLINE LIST ROUTINE                                                           
***********************************************************************         
LR       LA    R4,KEY                                                           
         USING TMKEY,R4                                                         
*                                                                               
         OC    KEY(TMKLENQ),KEY   FIRST TIME THROUGH?                           
         BNZ   LR10                                                             
*                                                                               
         MVI   TMKTYPE,TMKTYPEQ    AUTO TEAM KEY                                
         MVC   TMKYEAR,SAVEYEAR                                                 
         MVC   TMKSPR,SAVESPR                                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
LRLP     CLC   KEY(2),SAVEKEY                                                   
         BNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         OC    SAVESPR,SAVESPR     CHECK SPORT FILTER                           
         BZ    *+14                                                             
         CLC   TMKSPR,SAVESPR                                                   
         BNE   LRNEXT                                                           
*                                                                               
LRGO     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         MVC   LISTAR,BLANKS                                                    
         MVC   LSTSPRCD,TMKSPR     SPORT CODE                                   
         MVC   LSTTMCD,TMKTEAM     TEAM CODE                                    
         ICM   R0,3,TMKYEAR        YEAR CODE                                    
         LCR   R0,R0               FLIP THE BITS                                
         STCM  R0,3,LSTYEAR                                                     
*                                                                               
         LA    R6,TMFSTEL          A(FIRST ELEMENT)                             
         MVI   ELCODE,TMMNEQ       MAIN ELEMENT                                 
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TMMNED,R6                                                        
         MVC   LSTWIN(3),TMMNWIN      WINS                                      
         MVC   LSTLOS(3),TMMNLOS      LOSSES                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRNEXT   GOTO1 SEQ                 NEXT TEAM RECORD                             
         B     LRLP                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R5,P                                                             
         USING PLINE,R5                                                         
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TMKEY,R4                                                         
         MVI   TMKTYPE,TMKTYPEQ    TEAM KEY                                     
         MVC   TMKSPR,SAVESPR                                                   
         MVC   TMKYEAR,SAVEYEAR                                                 
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                FIRST RECORD                                 
         MVC   SAVEYR2,TMKYEAR                                                  
         MVI   FIRSTIME,C'Y'                                                    
*                                                                               
PRLP     CLC   KEY(2),SAVEKEY                                                   
         BNE   EXIT                                                             
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R4,KEY                                                           
         OC    SAVEYEAR,SAVEYEAR   CHECK SPORT FILTER                           
         BZ    PRSPRFL                                                          
         CLC   TMKYEAR,SAVEYEAR                                                 
         BE    PRSPRFL                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRX                                                              
*                                                                               
PRSPRFL  OC    SAVESPR,SAVESPR     CHECK SPORT FILTER                           
         BZ    *+14                                                             
         CLC   TMKSPR,SAVESPR                                                   
         BNE   PRNEXT                                                           
*                                                                               
         CLC   TMKYEAR,SAVEYR2                                                  
         BE    NOPGBRK                                                          
         MVI   FORCEHED,C'Y'                                                    
*        GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   SAVEYR2,TMKYEAR                                                  
         B     PRSKIP02                                                         
*                                                                               
NOPGBRK  CLI   FIRSTIME,C'Y'                                                    
         MVI   FIRSTIME,C'N'                                                    
         BE    PRSKIP02                                                         
         OC    ABOX,ABOX                                                        
         BZ    PRSKIP02                                                         
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
*                                                                               
PRSKIP02 GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         MVC   PSPR,TMKSPR         SPORT CODE                                   
         MVC   PTEAM,TMKTEAM       TEAM CODE                                    
         GOTO1 VLDSPR,DMCB,PSINF,L'PSINF,TMKSPR,0                               
         GOTO1 VLDTEAM,DMCB,PTINF,TMKSPR,TMKTEAM,0,L'PTINF                      
                                                                                
         LA    R6,TMFSTEL          A(FIRST ELEMENT)                             
         MVI   ELCODE,TMMNEQ       MAIN ELEMENT                                 
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TMMNED,R6                                                        
         MVC   PWIN,TMMNWIN      WINS                                           
         MVC   PLOS,TMMNLOS      LOSSES                                         
         DROP  R6                                                               
*                                                                               
         CLI   SFRPP,C'Y'          PRINT PLAYERS?                               
         BNE   PRNEXT                                                           
*                                                                               
         LA    R6,TMFSTEL          A(FIRST ELEMENT)                             
         USING TMPLREL,R6                                                       
         MVI   ELCODE,TMPLREQ      PLAYER ELEMENT CODE                          
         BAS   RE,FIRSTEL          ANY PLR FIELDS?                              
         BE    PRPLRLP                                                          
         GOTO1 SPOOL,DMCB(R8)                                                   
         B     PRNEXT                                                           
*                                                                               
* OUTPUT ID, NUMBER, POSITION, AND PLAYER INFO                                  
PRPLRLP  BNE   PRNEXT                                                           
         MVC   PID,TMPLRID         ... ID TO FIELD                              
         MVC   PNUM,TMPLRNUM       ... NUMBER TO FIELD                          
         MVC   PPOS,TMPLRPOS       ... POSITION TO FIELD                        
         GOTO1 DISPNAM,DMCB,PNAM,TMPLRID  ... NAME,DEPT TO FIELD                
         DROP  R5                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SEND LINE TO SCREEN                          
         MVI   ELCODE,TMPLREQ                                                   
         BAS   RE,NEXTEL                                                        
         B     PRPLRLP                                                          
*                                                                               
PRNEXT   MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 NEXT TEAM RECORD                             
         B     PRLP                                                             
*                                                                               
PRX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
******************************************************************              
* DISPLAY PLAYER INFO                                                           
******************************************************************              
* P1 = A(DEST)                                                                  
* P2 = A(ID)                                                                    
DISPNAM  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         USING PLRD,R2                                                          
         MVC   SAVEKEY2,KEY                                                     
         XC    KEY,KEY             BUILD A KEY FOR PERSON REC                   
         LA    R5,KEY                                                           
         USING PLAKEY,R5                                                        
         MVC   PLAKID,0(R3)                                                     
         MVI   PLAKTYPE,PLAKTYPQ                                                
         MVC   SAVEKEY3,KEY                                                     
         GOTO1 HIGH                                                             
         CLC   PLAKEY,SAVEKEY3     RECORD FOUND? IF NOT = BAD!!                 
         BE    PRSKIP01                                                         
         MVC   PLRNAM(42),=C'*** INVALID PROFS ID -- REC NOT FOUND ***'         
         B     DISPPX                                                           
                                                                                
PRSKIP01 MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
         L     R5,AIO                                                           
*                                                                               
         LA    R6,PLAFSTEL         ADDR OF FIRST ELEMENT                        
         MVI   ELCODE,PLANAMQ      GET NAME                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PLRNAM,PLANAME-PLANAMD(R6)                                       
*                                                                               
         LA    R6,PLAFSTEL         ADDR OF FIRST ELEMENT                        
         MVI   ELCODE,PLAEXTLQ     GET EXTENSION NUMBER                         
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PLREXT,PLAEXTN-PLAEXTD(R6)                                       
*                                                                               
         LA    R6,PLAFSTEL         ADDR OF FIRST ELEMENT                        
         MVI   ELCODE,PLADEPLQ     GET DEPT                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PLRDEPT,PLADEPT-PLADEPD(R6)                                      
*                                                                               
DISPPX   MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY2                                                     
         XIT1                                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* HEAD HOOK                                                                     
***********************************************************************         
HOOK     NTR1                                                                   
         OC    ABOX,ABOX                                                        
         BZ    HEADER                                                           
         L     R4,ABOX             A(BOX DSECT)                                 
         USING BOXD,R4                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+55,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+29,C'C'                                                  
         MVI   BOXCOLS+51,C'C'                                                  
         MVI   BOXCOLS+61,C'R'                                                  
         CLI   SFRPP,C'N'                                                       
         BE    HEADER                                                           
         MVI   BOXCOLS+61,C'C'                                                  
         MVI   BOXCOLS+130,C'R'                                                 
         DROP  R4                                                               
*                                                                               
HEADER   MVI   H3,0                                                             
         MVC   H4(6),=C'YEAR '''                                                
         LA    R4,KEY                                                           
         ICM   R0,3,TMKYEAR-TMKEY(R4)  YEAR CODE                                
         LCR   R0,R0               FLIP THE BITS                                
         STCM  R0,3,H4+6                                                        
         MVI   H7,0                                                             
         MVI   H9,0                                                             
         LA    R5,H10                                                           
         USING PLINE,R5                                                         
         MVC   PSPR,=C'CD'                                                      
         MVC   PSINF(4),=C'NAME'                                                
         MVC   PSINF+L'PSINF-7(6),=C'SEASON'                                    
         MVC   PTEAM,=C'CD'                                                     
         MVC   PTINF(4),=C'NAME'                                                
         MVC   PWIN,=C'WIN'                                                     
         MVC   PLOS,=C'LOS'                                                     
         CLI   SFRPP,C'Y'          IF NO PLAYER INFO, DONT PRINT                
         BNE   HOOKX               PLAYER INFO HEADING                          
*                                                                               
         MVC   H8+70(7),=C'PLAYERS'                                             
         MVC   PNUM(3),=C'NO.'                                                  
         MVC   PPOS(3),=C'POS'                                                  
         MVC   PID(8),=C'PROFS ID'                                              
         LA    R5,PNAM                                                          
         DROP  R5                                                               
         USING PLRD,R5                                                          
         MVC   PLRNAM(4),=C'NAME'                                               
         MVC   PLRDEPT(4),=C'DEPT.'                                             
         MVC   PLREXT(4),=C'EXT.'                                               
         DROP  R5                                                               
HOOKX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
HEDSPECS SSPEC H1,1,C'PETER TEAMS'                                              
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,51,C'PETER`S SPORT TEAM REPORT'                               
         SSPEC H2,51,C'--------------------------'                              
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         SSPEC H8,12,C'SPORT'                                                   
         SSPEC H8,37,C'TEAM'                                                    
         SSPEC H8,54,C'RECORD'                                                  
         DC    X'00'                                                            
***********************************************************************         
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         MVC   CONHEAD,MISSMSG                                                  
         GOTO1 ERREX2                                                           
MISSMSG  DC    CL60'MUST ENTER THIS REQUIRED FIELD'                             
*                                                                               
INVTEAM  MVC   CONHEAD,TEAMMSG                                                  
         GOTO1 ERREX2                                                           
TEAMMSG  DC    CL60'ERROR: INVALID TEAM CODE'                                   
*                                                                               
NUMERIC  CLI   5(R2),0             ANYTHING?                                    
         BER   RE                                                               
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNZR  RE                                                               
         MVC   CONHEAD,=CL60'ERROR: INVALID NUMBER'                             
         GOTO1 ERREX2                                                           
*                                                                               
INVSPR   MVC   CONHEAD,SPRMSG                                                   
         GOTO1 ERREX2                                                           
SPRMSG   DC    CL60'ERROR: INVALID SPORT CODE'                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
BLANKS   DS    CL132' '                                                         
         SPACE 3                                                                
*                                                                               
       ++INCLUDE CTGENPPAPC                                                     
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTSFM65D                                                       
         EJECT                                                                  
       ++INCLUDE CTGENPPAPD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENRLIAD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENSMUR                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB5D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         SPACE 5                                                                
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
ACURSOR  DS    A                   FORCE CURSOR HERE                            
SAVEKEY  DS    XL32                                                             
SAVEKEY2 DS    XL32                                                             
SAVEKEY3 DS    XL32                                                             
SAVETREC DS    XL1000                                                           
SAVESREC DS    XL1000                                                           
SAVESPR  DS    CL2                 KEY SPORT CODE                               
SAVETEAM DS    CL2                     TEAM CODE                                
SAVEYEAR DS    CL2                     YEAR CODE                                
SAVEYR2  DS    CL2                                                              
SAVEELEM DS    A                                                                
FIRSTIME DS    C                                                                
         SPACE 5                                                                
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LSTYEAR  DS    CL2                                                              
         DS    CL1                                                              
         DS    CL3                                                              
LSTSPRCD DS    CL2                                                              
         DS    CL6                                                              
LSTTMCD  DS    CL2                                                              
         DS    CL5                                                              
LSTWIN   DS    CL4                                                              
         DS    CL3                                                              
LSTLOS   DS    CL6                                                              
         SPACE 3                                                                
PLINE    DSECT                                                                  
         DS    CL2                                                              
PSPR     DS    CL2                                                              
         DS    CL1                                                              
PSINF    DS    CL23                                                             
         DS    CL3                                                              
PTEAM    DS    CL2                                                              
         DS    CL1                                                              
PTINF    DS    CL16                                                             
         DS    CL3                                                              
PWIN     DS    CL3                                                              
         DS    CL1                                                              
PLOS     DS    CL3                                                              
         DS    CL3                                                              
PNUM     DS    CL3                                                              
         DS    CL3                                                              
PPOS     DS    CL2                                                              
         DS    CL3                                                              
PID      DS    CL8                                                              
         DS    CL1                                                              
PNAM     DS    CL38                                                             
         SPACE 3                                                                
PLRD     DSECT                                                                  
PLRNAM   DS    CL20                                                             
         DS    CL2                                                              
PLREXT   DS    CL4                                                              
         DS    CL2                                                              
PLRDEPT  DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098SMCTSFREF 05/01/02'                                      
         END                                                                    
