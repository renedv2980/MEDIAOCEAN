*          DATA SET CTSFM30    AT LEVEL 027 AS OF 05/29/03                      
*PHASE TA0A30A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM30 -- SCROLLER DATATYPE MAINTENANCE/LIST/REPORT *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SCROLLER DATATYPE RECORDS ON GENDIR/GENFIL *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMD0 (MAINTENANCE)                        *         
*                        CTSFME0 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED DATATYPE RECORDS, LIST, OR REPORT.           *         
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
         TITLE 'TA0A30 - AGENCY OF RECORD RECORDS MAINT/LIST/REPORT'            
TA0A30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A30**,R7,RR=R3                                              
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
         DS    0H                                                               
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
         CLI   MODE,XRECPUT        CHANGED RECORDS, UPDATE CHG DATE             
         BE    DR                                                               
         CLI   MODE,XRECADD        ADDED RECORD, SET CREATE DATE                
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* VALIDATE KEY                                                                  
* ********************************************************************          
*-------------------------------------------------                              
*VALIDATE KEY: MAINTENANCE SCREEN (MNT..)                                       
*-------------------------------------------------                              
VK       XC    SYS,SYS               CLR VARIABLES FOR KEY                      
         XC    MED,MED                                                          
         XC    AOR,AOR                                                          
         XC    ADV,ADV                                                          
         XC    AGY,AGY                                                          
*                                                                               
         CLI   ACTEQU,ACTLIST        LIST ACTION?                               
         BE    VKLST20                                                          
*                                                                               
         LA    R2,MNTSYSH            SYSTEM FIELD                               
         GOTO1 ANY                   INPUT REQUIRED                             
         CLI   MNTSYS,C'P'           P IS ONLY VALID SYSTEM                     
         BNE   ERRINVL               IF NOT 'P', INVALID                        
         MVI   SYS,C'P'              SAVE CODE                                  
*                                                                               
         LA    R2,MNTMEDH            MEDIA FIELD                                
         GOTO1 ANY                   INPUT REQUIRED                             
         CLI   MNTSYS,C'*'           *=ALL SAVE AS X'FF'                        
         BNE   *+8                   IF NOT '*', BRANCH AROUND CHANGE           
         MVI   WORK,X'FF'            CHANGE TO X'FF'                            
         MVC   MED,WORK              SAVE MEDIA CODE                            
*                                                                               
         LA    R2,MNTAORH            AOR FIELD                                  
         GOTO1 ANY                   INPUT REQUIRED                             
         BAS   RE,VAGCY              VALIDATE AOR CODE                          
         MVC   AOR,MNTAOR            IF WE RTN, CODE IS VALID                   
         MVC   PRTAOR,SYSNAM         SAVE PRT NAME E.G. PRNT6                   
         MVC   PRNAOR,SYSNUM         SAVE PRT SYSTEM NUMBER                     
*                                                                               
         LA    R2,MNTADVH            ADVERTISER FIELD                           
         GOTO1 ANY                   INPUT REQUIRED                             
         CLI   5(R2),2               MORE THAN 2 CHARS INPUT?                   
         BL    ERRINVL               NO, INVALID INPUT                          
         MVC   ADV,WORK              YES, SAVE ADVERTISER CODE                  
*                                                                               
         LA    R2,MNTAGYH            AGENCY FIELD                               
         GOTO1 ANY                   INPUT REQUIRED                             
         BAS   RE,VAGCY              VALIDATE AGENCY CODE                       
         MVC   AGY,MNTAGY            IF WE RTN, CODE IS VALID                   
         MVC   PRTAGY,SYSNAM         SAVE PRT NAME E.G. PRNT6                   
         MVC   PRNAGY,SYSNUM         SAVE PRT SYSTEM NUMBER                     
*                                                                               
         B     VKKEY30                                                          
         EJECT                                                                  
*                                                                               
* ---------------------------------------------                                 
* VALIDATION OF KEY:  FOR LIST SCREEN (LST..)                                   
* ---------------------------------------------                                 
VKLST20  LA    R2,LSTSYSH            SYSTEM FIELD REQUIRED                      
         GOTO1 ANY                                                              
         MVC   SYS,LSTSYS            YES, SAVE FOR KEY                          
         CLI   LSTMEDH+5,0           ANY MEDIA INPUT?                           
         BE    VKLST30               NO, TEST AOR FIELD                         
         CLI   LSTMED,C'*'           WAS AN * ENTERED?                          
         BNE   *+8                   NO, SKIP CONVERSION TO X'FF'               
         MVI   LSTMED,X'FF'          *=ALL=X'FF'                                
         MVC   MED,LSTMED            SAVE MED FOR KEY                           
VKLST30  CLI   LSTAORH,0             ANY INPUT IN AOR FIELD?                    
         BE    VKKEY30               NO, GO BUILD THE KEY                       
         ZIC   R1,LSTAORH+5          GET LENGTH OF INPUT                        
         BCTR  R1,0                  DECR FOR EX                                
         EXMVC R1,AOR,LSTAOR         SAVE AOR START AT FOR KEY                  
         EJECT                                                                  
*                                                                               
* --------------------------------------------------------                      
* BUILD AOR KEY:  FROM MNT/LST SCREEN                                           
* --------------------------------------------------------                      
VKKEY30  LA    R4,KEY                                                           
         USING ADVKEYD,R4            BUILD KEY                                  
         XC    KEY,KEY               CLEAR THE KEY                              
         MVI   ADVREC,ADVRECQ        RECORD CODE- 1ST BYTE                      
         MVI   ADVTYP,ADVTYPQ        RECORD TYPE- 2ND BYTE                      
         MVC   ADVSYS,SYS            SYSTEM (P)                                 
         MVC   ADVAOR,AOR                                                       
         MVC   ADVADV,ADV                                                       
         MVC   ADVAGY,AGY                                                       
         MVC   ADVMED,MED                                                       
         DROP  R4                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* --------------------------------------------------------------------          
* VALIDATE 3 CHARACTER PRINCIPLE ADVERTISER CODE.                               
* PROCEDURE CALLED WITH R2 POINTING TO ADVERTISER HEADER.                       
* --------------------------------------------------------------------          
*                                                                               
VADVR    CLI   5(R2),1               2 OR MORE CHAR INPUT?                      
         BH    *+14                                                             
         MVC   GERROR,=AL2(INVALID)  LESS THEN 2 CHARS NO GOOD                  
         B     VSFMERR                                                          
         OI    8+2(R2),X'40'         BLANK PAD 3RD CHAR OF ADVR                 
         MVI   5(R2),3               RESET LENGTH TO 3 (INCLUDE SPACE)          
         B     RE                    RETURN TO CALLER                           
         EJECT                                                                  
*                                                                               
* --------------------------------------------------------------------          
* VALIDATE 2 CHARACTER AGENCY CODE.                                             
* PROCEDURE CALLED WITH R2 POINTING TO AGENCY FIELD HEADER.                     
* --------------------------------------------------------------------          
VAGCY    NTR1                        SAVE REGISTERS                             
         CLI   5(R2),2               2 CHARS INPUT?                             
         BNE   VINVAGY               NO, INVALID INPUT FIELD                    
*                                                                               
         XC    CTKEY,CTKEY           BUILD CTFILE KEY                           
         LA    R6,CTKEY                                                         
         USING CT5REC,R6             CTFILE KEY DSECT                           
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,8(R2)                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,AIO2                
         TM    DMCB+8,X'10'          RECORD FOUND?                              
         BO    VINVAGY               NO,INVALID INPUT                           
*                                                                               
         MVC   DATADISP,=H'28'       25BYTE DISP FOR CTFILE KEY                 
         L     R6,AIO2               PT TO RECORD OBTAINED                      
         MVI   ELCODE,CTSYSELQ       FIND SYSTEM NUMBER ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                   IF THERE, CONTINUE                         
         DC    H'0'                  ELSE MAJOR BUG IN ADVTSR RECORD            
*                                                                               
         USING CTSYSD,R6             ELEMENT SYSTEM NUMBER DSECT                
VAGCY15  CLI   CTSYSNUM,X'04'        MAKE SURE ON PRINT4 SYSTEM                 
         BE    VAGCY20               FOUND, EXTRACT SYS NAME CODE               
         BAS   RE,NEXTEL             ELSE LOOK THRU OTHER X'21' ELMNTS          
         BE    VAGCY15               IF ANOTHER X'21' ELMT FOUND LOOP           
         B     VINVAGY               ELSE INVALID AGENCY                        
*                                                                               
VAGCY20  MVC   SYSNUM,CTSYSSE        SAVE SYSTEM NUMBER CODE                    
         L     R1,SYSPARMS           A(PARAMS)                                  
         L     R1,0(R1)              A(SYSFACS)                                 
         USING SYSFACD,R1                                                       
         L     R5,VSELIST            A(SYSTEM EXECUTIVE LIST)                   
         DROP  R1                                                               
         USING SELISTD,R5                                                       
         LH    R6,0(R5)              LENGTH OF TABLE ENTRY                      
         L     R7,2(R5)              A(END OF TABLE)                            
         LA    R5,6(R5)              A(FIRST ENTRY)                             
*                                                                               
         CLC   SYSNUM,SESYS          MATCH ON SPECIFIC NUMBER?                  
         BE    *+10                  YES                                        
         BXLE  R5,R6,*-10            NO -- TRY NEXT ENTRY                       
         DC    H'0'                                                             
         MVC   SYSNAM,SENAME         PULL OUT 5TH CHAR OF SYSTEM NAME           
         DROP  R5                                                               
         B     VAGCYX                DONE VALIDATING                            
*                                                                               
VINVAGY  MVC   GERROR,=AL2(INVAGCY)  INVALID AGENCY                             
         B     VSFMERR                                                          
*                                                                               
VAGCYX   XIT1                        RESTORE REGISTERS                          
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* VALIDATE RECORD                                                               
* ********************************************************************          
VR       L     R4,AIO               BUILD RECORD                                
         USING ADVKEYD,R4                                                       
         LA    R1,MNTCLSTH           LAST CLIENT LINE ON SCREEN                 
         ST    R1,ENDADR             SAVE FOR LOOPING LATER ON                  
*                                                                               
*---SAVE AWAY KEY FIELDS (AGY,AOR,MED ETC) ?????                                
*                                                                               
*/AGENCY ELEMENT/                                                               
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,AGYELQ        AGENCY ELEMBENT                             
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM             CLEAR ELEMENT                              
         LA    R6,ELEM                                                          
         USING AGYD,R6                                                          
         MVI   AGYEL,AGYELQ          AGENCY ELEMENT                             
         MVI   AGYLN,AGYLNQ          AGY LENGTH                                 
         MVC   AGYPRT,PRTAGY         AGY PRINT NAME                             
         MVC   AGYPRN,PRNAGY         AGY PRINT NUMBER                           
         LA    R2,MNTAGNMH           PT TO NAME HEADER                          
         GOTO1 ANY                   INPUT REQUIRED                             
         ZIC   R1,5(R2)              PUT LENGTH OF INPUT IN R1                  
         BCTR  R1,0                  DECR FOR EX                                
         EXMVC R1,AGYNM,MNTAGNM      SAVE AGY NAME IN ELEMENT                   
*                                                                               
         CLI   MNTCTLH+5,0           ANY CONTROL CHARS ENTERED?                 
         BE    VR5                   NO, ADD AGY ELEMENT                        
         ZIC   R1,MNTCTLH+5          PUT LENGTH OF INPUT IN R1                  
         BCTR  R1,0                  DECR FOR EX                                
         EXMVC R1,AGYCNTL,MNTCTL     SAVE CONTROL CHARACTERS                    
*                                                                               
VR5      CLI   MNTFLTRH+5,0          ANY FILTER CHARS ENTERED?                  
         BE    VR8                   NO, ADD AGY ELEMENT                        
         LA    R2,MNTFLTRH                                                      
         CLI   5(R2),3                                                          
         BNE   ERRINVL                                                          
         ZIC   R1,5(R2)              PUT LENGTH OF INPUT IN R1                  
         BCTR  R1,0                  DECR FOR EX                                
         EXMVC R1,AGYFLTR,MNTFLTR    SAVE FILTER CHARACTERS                     
*                                                                               
VR8      GOTO1 ADDELEM               ADD ELEMENT                                
*                                                                               
*/CLIENT ELEMENT/                                                               
         MVI   ELCODE,CLTELQ         AGENCY ELEMBENT                            
         GOTO1 REMELEM               BUILD ELEMENT FROM SCRATCH                 
         LA    R6,ELEM               PT TO BUILD ELEMENT AREA                   
         USING CLTD,R6               USE CLIENT DSECT                           
         MVI   CLTLN,CLTLNQ          AGY LENGTH                                 
*                                                                               
         XC    TMPEND,TMPEND       CLEAR END DATE FIELD                         
         LA    R2,MNTSTRH            PT TO START-DATE FIELD                     
         GOTO1 ANY                   START-DATE REQUIRED                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),TMPSTR                                     
         CLI   CLTSTR,C'0'           VALID DATE?                                
         BE    ERRINVL               PRINT INVALID DATE FORMAT MSG              
*                                                                               
         LA    R2,MNTENDH                                                       
         CLI   5(R2),0               END-DATE ENTERED?                          
         BE    VR9                   NO                                         
         GOTO1 DATVAL,DMCB,(0,8(R2)),TMPEND                                     
         CLI   CLTEND,C'0'           VALID DATE?                                
         BE    ERRINVL               PRINT INVALID DATE FORMAT MSG              
*                                                                               
VR9      LA    R2,MNTCLTH            PT TO FIRST CLIENT HEADER                  
         GOTO1 ANY                   AT LEAST ONE CLIENT IS REQUIRED            
*                                                                               
VR10     XC    ELEM,ELEM             CLEAR ELEMENT BUILD AREA                   
         CLC   8(6,R2),=C'DELETE'    DELETE CLIENT ?                            
         BE    *+12                  YES                                        
         CLI   5(R2),0               ANY MORE CLIENT ELEMENTS                   
         BNE   VR15                  NO INVALID INPUT                           
         LA    R2,MNTCLT2H-MNTCLTH(R2)  BUMP TO NEXT CLIENT FIELD               
         C     R2,ENDADR             BOTTOM OF SCREEN?                          
         BNH   VR10                  NOT YET- KEEP PROCESSING                   
         B     VRX                   YES, NO MORE CLIENTS                       
*                                                                               
VR15     XC    CLTADV,CLTADV                                                    
         CLI   5(R2),2                                                          
         BNE   *+14                                                             
         MVC   CLTAGY(2),8(R2)       SAVE 2 CHAR CLIENT CODE                    
         B     VR40                                                             
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VR18                                                             
         CLI   2+8(R2),C'='                                                     
         BE    ERRINVL                                                          
         MVC   CLTAGY(3),8(R2)       SAVE 3 CHAR CLT CODE                       
         B     VR40                                                             
*                                                                               
VR18     CLI   5(R2),5               AT LEAST 5 CHARS 'XX=YY'                   
         BL    ERRINVL               NO,INVALID INPUT                           
         CLI   2+8(R2),C'='          IS 3RD CHAR '=' SIGN                       
         BNE   VR20                  NO, TRY 4TH CHAR                           
         MVC   CLTAGY(2),8(R2)       YES, SAVE 2 CHAR CLT AGY CODE              
         ZIC   R1,5(R2)              LENGTH OF INPUT FIELD                      
         SH    R1,=H'4'              -4 FOR 2 CHAR AGY,'=',AND EX               
         EXMVC R1,CLTADV,3+8(R2)     SAVE ADVERTISER CODE                       
         B     VR40                  ADD ELEMENT                                
*                                                                               
VR20     CLI   3+8(R2),C'='          IS 4TH CHAR AN '=' SIGN                    
         BNE   ERRINVL               NO, INVALID INPUT                          
         CLI   5(R2),5               AT LEAST 6 CHARS INPUT?                    
         BL    ERRINVL               NO, INVALID INPUT                          
         MVC   CLTAGY(3),8(R2)       3 CHAR CLIENT AGY CODE                     
         ZIC   R1,5(R2)              LENGTH OF INPUT                            
         SH    R1,=H'5'              -5:3 CHAR AGY,'=',EX                       
         EXMVC R1,CLTADV,4+8(R2)     SAVE ADVERTISER CODE                       
*                                                                               
VR40     MVI   ELCODE,CLTELQ         PUT CLIENT ELEMENT CODE INTO ELEM          
         MVI   CLTEL,CLTELQ          CLIENT ELEMENT                             
         MVI   CLTLN,CLTLNQ          SAVE CLIENT ELEMENT FIXED LENGTH           
         OC    CLTAGY,SPACES         BLANK PAD AGENCY FIELD                     
         OC    CLTADV,CLTADV         IF FIELD IS EMPTY-KEEP AS BIN 0            
         BZ    *+10                                                             
         OC    CLTADV,SPACES         BLAND PAD ADVERTISER FLD                   
         GOTO1 DATCON,DMCB,TMPSTR,(3,CLTSTR)    SAVE AS BINARY                  
         OC    TMPEND,TMPEND       IS IT NULLS?                                 
         BZ    VR45                YES                                          
         GOTO1 DATCON,DMCB,TMPEND,(3,CLTEND)    SAVE AS BINARY                  
VR45     GOTO1 ADDELEM               ADD CLIENT ELEMENT                         
*                                                                               
         ZIC   R1,0(R2)              LENGTH OF CLIENT FIELD                     
         AR    R2,R1                 BUMP TO NEXT CLIENT FIELD                  
         C     R2,ENDADR             HAVE WE LOOKED @ ALL CLIENT FLDS?          
         BNH   VR10                  NO, PROCESS NEXT CLIENT FIELD              
*                                                                               
VRX      B     XIT                   DISPLAY THE RECORD                         
         DROP  R4                                                               
         EJECT                                                                  
* ********************************************************************          
* DISPLAY RECORD                                                                
* ********************************************************************          
DR       LA    R0,MNTTAGH            LAST FIELD ON SCREEN                       
         LA    R2,MNTCLTH            1ST CLIENT FIELD                           
         MVC   MNTAGNM,SPACES         CLEAR AGENCY NAME FIELD                   
         OI    MNTAGNMH+6,X'80'       TRANSMIT                                  
         MVC   MNTCTL,SPACES         CLEAR CTL CHARS FIELD                      
         OI    MNTCTLH+6,X'80'       TRANSMIT                                   
         MVC   MNTFLTR,SPACES        CLEAR FLTR CHARS FIELD                     
         OI    MNTFLTRH+6,X'80'      TRANSMIT                                   
         MVC   MNTSTR,SPACES         CLEAR STR DATE CHAR FIELD                  
         OI    MNTSTRH+6,X'80'       TRANSMIT                                   
         MVC   MNTEND,SPACES         CLEAR END DATE CHAR FIELD                  
         OI    MNTENDH+6,X'80'       TRANSMIT                                   
*                                                                               
DR10     ZIC   R1,0(R2)              LENGTH OF FIELD                            
         TM    1(R2),X'02'           TEST IF THERE'S AN EXTENSION               
         BZ    *+8                   NO EXTN                                    
         SH    R1,=H'8'              YES, REDUCE LENGTH BY EXTN LENGTH          
         SH    R1,=H'9'              -8 FOR FIELD HDR, -1 FOR EX                
         EXMVC R1,8(R2),SPACES       BLANK OUT FIELD                            
         OI    6(R2),X'80'           TRANSMIT                                   
         ZIC   R1,0(R2)              RESTORE LENGTH FOR BUMPING                 
         AR    R2,R1                 BUMP TO NEXT FLD HDR                       
         CR    R2,R0                 HAVE WE REACHED THE END OF SCREEN?         
         BL    DR10                  NO, KEEP CLEARING                          
*                                                                               
*GET AGY ELEMENT                                                                
         L     R6,AIO                                                           
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,AGYELQ                                                    
         BAS   RE,GETEL              GET AGY ELEMENT                            
         BE    *+6                   AGY ELEMENT REQUIRED                       
         DC    H'0'                  NOT THERE, BUG!                            
         USING AGYD,R6                                                          
         MVC   MNTAGNM,AGYNM                                                    
         OI    MNTAGNMH+6,X'80'                                                 
         MVC   MNTCTL,AGYCNTL                                                   
         OI    MNTCTLH+6,X'80'                                                  
         MVC   MNTFLTR,AGYFLTR                                                  
         OI    MNTFLTRH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         LA    R0,MNTCLSTH           R0=LAST CLIENT FIELD                       
         ST    R0,ENDADR             SAVE FOR END CONDITION COMPARE             
         L     R6,AIO                                                           
         USING CLTD,R6                                                          
         MVI   ELCODE,CLTELQ                                                    
         BAS   RE,GETEL              GET A CLIENT ELEMENT                       
         BNE   DRX                   DONE PROCESSING CLIENT ELEM?               
*                                                                               
         LA    R2,MNTSTRH          PT TO START DATE FIELD                       
         GOTO1 DATCON,DMCB,(3,CLTSTR),(8,8(R2))                                 
*                                                                               
         LA    R2,MNTENDH                                                       
         OC    CLTEND,CLTEND         IS THERE AN END DATE?                      
         BZ    DR19                  NO, DON'T CALL DATCON                      
         GOTO1 DATCON,DMCB,(3,CLTEND),(8,8(R2))                                 
*                                                                               
DR19     LA    R2,MNTCLTH            PT TO FIRST CLIENT FIELD                   
DR20     MVC   8(3,R2),CLTAGY        PRINT CLIENT AGY CODE                      
         OC    CLTADV,CLTADV                                                    
         BZ    DR25                                                             
         MVI   3+8(R2),C'='          PUT AN = SIGN BETW AGY=ADV                 
         MVC   4+8(3,R2),CLTADV      PRINT CLIENT AGY CODE                      
DR25     OI    6(R2),X'80'           TRANSMIT                                   
*                                                                               
         ZIC   R1,0(R2)              BUMP TO NEXT (CLT) FIELD                   
         AR    R2,R1                                                            
         C     R2,ENDADR             HAVE WE FILLED THE SCREEN?                 
         BH    DRX                   YES, WE ARE DONE                           
         BAS   RE,NEXTEL             NO, GET NEXT CLIENT ELEMENT                
         BE    DR20                  LOOP BACK                                  
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* DISPLAY KEY                                                                   
* ********************************************************************          
DK       L     R4,AIO                SELECTED RECORD                            
         USING ADVKEYD,R4                                                       
         MVC   MNTSYS,ADVSYS         SYSTEM                                     
         OI    MNTSYSH+6,X'80'                                                  
         MVC   MNTMED,ADVMED         MEDIA                                      
         OI    MNTMEDH+6,X'80'                                                  
         MVC   MNTAOR,ADVAOR         AOR                                        
         OI    MNTAORH+6,X'80'                                                  
         MVC   MNTADV,ADVADV         ADVERTISER                                 
         OI    MNTADVH+6,X'80'                                                  
         MVC   MNTAGY,ADVAGY         AGENCY                                     
         OI    MNTAGYH+6,X'80'                                                  
*                                                                               
*GET AGY ELEMENT- EXTRACT SYSTEM CODE (PRNT#)                                   
         L     R6,AIO                                                           
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,AGYELQ                                                    
         BAS   RE,GETEL              GET AGY ELEMENT                            
         BE    *+6                   AGY ELEMENT REQUIRED                       
         DC    H'0'                  NOT THERE, BUG!                            
         USING AGYD,R6                                                          
         MVC   PRTAGY,AGYPRT         SAVE CODES SO THAT A CHG FROM LIST         
         MVC   PRNAGY,AGYPRN          SCREEN WILL NOT CLOBBER AGY ELMNT         
         DROP  R6                     WHICH SAVED CODE FROM ORIG VALKEY         
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* ON-SCREEN LIST                                                                
* ********************************************************************          
LR       LA    R4,KEY                                                           
         USING ADVKEYD,R4                                                       
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVI   ADVREC,ADVRECQ      RECORD CODE                                  
         MVI   ADVTYP,ADVTYPQ      RECORD TYPE                                  
         MVC   ADVSYS,SYS          START AT FIELDS                              
         MVC   ADVMED,MED                                                       
         MVC   ADVAOR,AOR                                                       
         MVC   ADVADV,ADV                                                       
         MVC   ADVAGY,AGY                                                       
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
LR30     CLC   KEY(2),SAVEKEY       SAME SYSTEM/PROGRAM?                        
         BNE   LRX                  NO MORE DATATYPES TO LIST                   
*                                                                               
         GOTO1 GETREC               GET THE SCROLLER DATATYPE RECORD            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         USING ADVKEYD,R4                                                       
*                                                                               
* BUILD LIST LINE                                                               
LBLD     MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LISTSYS,ADVSYS      SYSTEM                                       
         MVC   LISTMED,ADVMED      MEDIA                                        
         CLI   ADVMED,X'FF'        X'FF'=ALL USER ENTERS AS '*'                 
         BNE   *+8                 NOT ALL CHARACCTER                           
         MVI   LISTMED,C'*'        REPLACE X'FF' WITH '*'                       
         MVC   LISTAOR,ADVAOR      AOR                                          
         MVC   LISTADV,ADVADV      ADVERTISER                                   
         MVC   LISTAGY,ADVAGY      AGENCY                                       
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
* *********************************************************************         
* FIELD DECLARATIONS, DSECTS, SCREENS (ALL THE ++INKLUDEES)                     
* *********************************************************************         
*                                                                               
*                                                                               
ERRINVL  MVC   GERROR,=AL2(INVALID)  INVALID INPUT FIELD                        
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*                                                                               
RELO     DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMA7DA                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE FASELIST                                                       
         EJECT                                                                  
       ++INCLUDE FASYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
* SCREENS INCLUDED: CTSFMD0 AND CTSFME0, MAINT AND LIST RESPECTIVELY            
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD0D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFME0D                                                       
         EJECT                                                                  
       ++INCLUDE CTGENADVD                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
CTKEY    DS    CL25                CTFILE KEY                                   
SAVEKEY  DS    CL32                                                             
SYSNUM   DS    X                   1 CHAR SYSTEM NUMBER CODE                    
PRNAGY   DS    X                   AGENCY PRINT NUMBER                          
PRNAOR   DS    X                   AOR PRINT NUMBER                             
HLF      DS    H                   FOR MULTIPLY ETC INSTR                       
ENDADR   DS    A                   ADDRESS OF TABLE END                         
*                                                                               
SYS      DS    CL1                 SYSTEM -USED TO BUILD KEY                    
MED      DS    CL1                 MEDIA - "                                    
AOR      DS    CL2                 AOR   - "                                    
ADV      DS    CL3                 ADVERTISER -"                                
AGY      DS    CL3                 AGENCY - "                                   
SYSNAM   DS    CL5                 5TH CHAR OF TRANSLATED NAME                  
PRTAGY   DS    CL5                 AGENCY PRINT NAME                            
PRTAOR   DS    CL5                 AOR PRINT NAME                               
TMPSTR   DS    CL6                 TEMP SAVE START DATE FROM DATVAL             
TMPEND   DS    CL6                 TEMP SAVE END DATE FROM DATVAL               
DATATYPE DS    CL7                 DATATYPE NAME                                
*                                                                               
INVAGCY  EQU   289                 INVALID ALPHA AGENCY CODE                    
INVDUPL  EQU   303                 DUPLICATE RECORD (ACTLY ELEMNT)              
*                                                                               
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LISTSYS  DS    CL1                 SYSTEM                                       
         DS    CL5                                                              
LISTMED  DS    CL1                 MEDIA                                        
         DS    CL5                                                              
LISTAOR  DS    CL2                 AOR                                          
         DS    CL4                                                              
LISTADV  DS    CL3                 ADVERTISER                                   
         DS    CL3                                                              
LISTAGY  DS    CL2                 AGENCY                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027CTSFM30   05/29/03'                                      
         END                                                                    
