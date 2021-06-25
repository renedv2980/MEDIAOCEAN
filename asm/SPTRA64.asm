*          DATA SET SPTRA64    AT LEVEL 024 AS OF 03/06/07                      
*PHASE T21664B                                                                  
         TITLE 'T21664 PRODUCT EQUIV DISPLAY/CHANGE/ADD/DELETE/LIST'            
***********************************************************************         
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR                
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER & ELEM FOR DSECT                     
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO AIO                 
*        R7 - WORK                                                              
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*                                                                               
***********************************************************************         
*                                                                     *         
* LEV 14    MAR05/92 STOP SHOWING ACTIVE/INACTIVE                     *         
* LEV 15    APR30/92 ADD PRINTREP FEATURE                             *         
* LEV 16    JUN17/92 ADD AGENCY CODE TO RECORD                        *         
* LEV 17    JUL08/93 FORCE PEQACT TO Y                                *         
* LEV 18    MAR30/94 STRAFFIC                                         *         
* LEV 19 SMUR NOV10/99 USE RECUP FROM FACPAK                          *         
* LEV 20 SMUR OCT10/01 OFFICE AND OFFICE LIST SECURITY                *         
* LEV 21 SMUR JUL09/02 CLIENT STRING SECURITY                         *         
* LEV 23 SMUR JUL29/04 SOX                                            *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21664   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21664**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
         CLI   MODE,XRECDEL        AFTER DELETE RECORD                          
         BE    XDEL                                                             
         CLI   MODE,XRECREST       AFTER RESTORE RECORD                         
         BE    XREST                                                            
         CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    XADD                                                             
         CLI   MODE,XRECPUT        AFTER CHANGE RECORD                          
         BE    XADD                                                             
         SPACE                                                                  
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH          FIELD PTR FOR CLIENT                         
         XC    BCLT,BCLT           CLEAR BINARY CLIENT                          
         XC    BPRD,BPRD           CLEAR BINARY PRODUCT                         
         XC    QPRD,QPRD           CLEAR DESCRIP PROD                           
         CLI   5(R2),0             ANY ENTRY FOR CLIENT                         
         BNE   VK10                YES                                          
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK07                 NO                                          
         SPACE                                                                  
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
         SPACE                                                                  
VK07     CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VK20                NO ENTRY BUT NOT NEEDED FOR LIST             
         B     MISSERR             NO, MUST BE ENTRY                            
VK10     GOTO1 VALICLT                                                          
         SPACE                                                                  
         LA    R2,TRAPRODH         FIELD PTR FOR PRODUCT                        
         CLI   5(R2),0             ANY ENTRY?                                   
         BNE   VK15                YES                                          
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK20                PROD NOT NEEDED FOR LIST                     
         B     MISSERR             MUST BE ENTRY ON ADD                         
*                                                                               
VK15     OC    BCLT,BCLT           IF CLIENT NOT ENTERED                        
         BZ    MISSCLT              ERROR                                       
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK        PRODUCT POL INVALID                          
         BE    INVPRDER                                                         
*                                                                               
* MUST READ THROUGH PROD EQUV RECORDS (PASSIVE POINTER)                         
* MAKE SURE THAT BASE PRODUCT DOESN'T EXIST AS PROD. EQUIV.                     
*                                                                               
         MVC   AIO,AIO2            SWITCH AIO'S                                 
         LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING PEQKEY,R4                                                        
         MVC   PEQPID,=XL2'0AB7'   RECORD TYPE                                  
         MVC   PEQPAM,BAGYMD       A/M                                          
         MVC   PEQPCLT,BCLT        CLIENT                                       
         MVC   PEQPEPRD,WORK       PRODUCT                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            SWITCH BACK                                  
         CLC   KEY(8),KEYSAVE      IS BASE PRD AN EQUIV. PRD?                   
         BE    INVPRD1              YES -ERROR----                              
*                                                                               
         MVC   QPRD,WORK           SAVE EBCIC PRODUCT                           
         MVC   BPRD,WORK+3         SAVE BINARY PRODUCT                          
         EJECT                                                                  
* BUILD KEY *                                                                   
         SPACE                                                                  
VK20     LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING PEQKEY,R4                                                        
         MVC   PEQKID,=X'0A37'     RECORD TYPE                                  
         MVC   PEQKAM,BAGYMD       MOVE IN A/M                                  
         MVC   PEQKCLT,BCLT        MOVE IN CLIENT                               
         MVC   PEQKPRD,QPRD        MOVE PRODUCT INTO KEY                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE                                                                  
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         L     R6,AIO                                                           
         MVC   20(2,R6),AGENCY                                                  
         MVC   SVKEY,KEY           SAVE GENCON'S KEY                            
         LA    R3,15               MAX # OF ENTRIES FOR 1 SCREEN                
         LA    R2,TRAPRDAH         FIRST PROD EQUIV ON SCREEN                   
*                                                                               
         CLC   8(4,R2),=C'NONE'    DO THEY WANT EMPTY ELEMENTS                  
         BNE   VR02X               NO                                           
*                                                                               
         CLI   ACTNUM,ACTADD       IS THIS BRAND NEW REC                        
         BNE   VR01                                                             
         B     EXIT                                                             
*                                                                               
VR01     DS    0H                  MUST BE CHANGE -NEED TO DELETE               
         MVI   DELPASPT,1          RECORD AND ALL PASSIVE POINTERS              
         L     R6,AIO                                                           
         USING PEQDTAEL,R6                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                ADD EMPTY RECORD                             
VR02     MVC   SVPROD,PEQPROD                                                   
         BAS   R5,DELPRDEQ                                                      
         CLI   0(R6),X'10'         IS NEXT ONE TO BE DELETED TOO?               
         BE    VR02                                                             
         B     EXIT                ADD EMPTY RECORD                             
*                                                                               
VR02X    MVI   NOELEM,0            0= NO MORE ELEMENTS IN RECORD                
         XC    SVPROD,SVPROD       SAVE PROD EQUIV FOR DELETE                   
         XC    SCRNPRD(45),SCRNPRD     PRD EQUIV ON SCREEN                      
         L     R6,AIO                                                           
         USING PEQDTAEL,R6                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     VR07                                                             
*                                                                               
VR04     BAS   RE,NEXTEL                                                        
*                                                                               
VR07     BE    VR08                                                             
         MVI   NOELEM,1            NO MORE ELEMS IN REC =1                      
         B     VR10                VALIDATE OFF SCREEN                          
         SPACE                                                                  
VR08     MVI   DELPASPT,0          DELETE PASSIVE POINTER ONLY                  
         OC    8(3,R2),8(R2)       IS ELEM BLANKED OUT?                         
         BNZ   VR08K                                                            
         MVI   DELPASPT,1          DELETE BOTH                                  
         MVC   SVPROD,PEQPROD      SAVE PROD EQUIV FOR DELETE                   
         BAS   R5,DELPRDEQ         DELETE PASSIVE PTR & ELEM                    
         B     VR45                CHECK NEXT PAIR                              
*                                                                               
VR08K    OC    8(3,R2),SPACES                                                   
         CLC   8(3,R2),PEQPROD                                                  
         BE    VR08X                                                            
         CLC   8(4,R2),=C'NONE'                                                 
         BE    INVNONE             NONE INVALID HERE                            
         MVI   DELPASPT,0          DELETE PASSIVE PTR & CHANGE ELEM             
         MVC   SVPROD,PEQPROD      SAVE PROD EQUIV FOR DELETE                   
         BAS   R5,DELPRDEQ         GO DELETE                                    
         MVI   EQPRDFLG,1          1=NEW PROD. EQUIV                            
         B     VR10                                                             
         SPACE                                                                  
VR08X    ZIC   R1,0(R2)            SAME PROD. EQUIV - CHECK A/I/D FLAG          
         AR    R2,R1                                                            
*                                                                               
         ZIC   RE,5(R2)            LENGTH OF INPUT                              
         BCTR  RE,0                NEED -1 THEN ACTUAL FOR EX                   
         EX    RE,VRCLC1           IS IT EQUAL                                  
         BE    VR50                ELEEMENT OK, GET NEXT PAIR                   
         CLI   8(R2),C'D'          ACTION DELETE                                
         BE    VR09                                                             
         MVI   EQPRDFLG,1                                                       
         B     VR25                FLAG HAS BEEN CHANGED                        
         SPACE                                                                  
VR09     MVI   DELPASPT,1          DELETE BOTH                                  
         SR    R2,R1               POSITION BACK TO PROD. EQUIV                 
         MVC   SVPROD,PEQPROD      SAVE PROD EQUIV FOR DELETE                   
         BAS   R5,DELPRDEQ                                                      
         B     VR45                NEXT PAIR                                    
         SPACE                                                                  
VRCLC1   CLC   8(0,R2),PEQACT                                                   
         SPACE                                                                  
         EJECT                                                                  
* VALIDATION OF PRODUCT EQUIVALENT AND A/I/F FLAG                               
VR10     DS    0H                                                               
         CLI   5(R2),0             ANY PROD EQUIV ENTERED?                      
         BE    VR20                NO- CHECK FLAG?                              
         CLC   8(4,R2),=C'NONE'                                                 
         BE    INVNONE             NONE INVALID HERE                            
         CLI   5(R2),2                                                          
         BL    TRAPERR                                                          
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         MVI   DELPASPT,1                                                       
         MVI   EQPRDFLG,1                                                       
         SPACE                                                                  
* PRODUCT EQUIVALENT MUST BE IN SVCLIST                                         
         SPACE                                                                  
         L     R4,ASVCLIST                                                      
         OC    8(3,R2),SPACES                                                   
VR12     CLC   0(3,R4),8(R2)       DO WE HAVE MATCH                             
         BE    VR13                                                             
         LA    R4,4(,R4)           NEXT PRODUCT IN LIST                         
         CLI   0(R4),C' '          END OF LIST?                                 
         BH    VR12                                                             
         B     NOPRD               THIS PRODUCT DOESN'T EXIST                   
         SPACE                                                                  
* MUST READ THROUGH PROD EQUIV RECORDS                                          
* MAKING SURE THAT PROD EQUIV. INPUTTED IS NOT                                  
* A BASE PRODUCT                                                                
* * NEED TO SWITCH AIO'S **                                                     
VR13     MVC   AIO,AIO2            SWITCH FOR READING                           
         LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING PEQKEY,R4                                                        
         MVC   PEQKID,=X'0A37'     RECORD TYPE                                  
         MVC   PEQKAM,BAGYMD                                                    
         MVC   PEQKCLT,BCLT        MOVE IN CLIENT                               
         MVC   PEQKPRD,8(R2)       MOVE EQUIV PROD IN BASE PROD                 
         OC    PEQKPRD,SPACES                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLC   KEY(8),KEYSAVE                                                   
         BE    INVEPRD             PRODUCT EQUIV ALREADY IS BASE PROD           
         SPACE                                                                  
* CHECK IF PROD EQUIV SAME AS BASE PROD ON SCREEN                               
         OC    8(3,R2),SPACES                                                   
         CLC   8(3,R2),QPRD                                                     
         BE    INVEPRD                                                          
*                                                                               
* NOW NEED TO CHECK IF EQUIV. PRODUCT ALREADY EXISTS                            
*                                                                               
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         MVC   0(2,R4),=XL2'0AB7'   RECORD TYPE                                 
         MVC   2(1,R4),BAGYMD                                                   
         MVC   3(2,R4),BCLT                                                     
         MVC   5(3,R4),8(R2)                                                    
         OC    5(3,R4),SPACES                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            SWITCH BACK                                  
         CLC   KEY(8),KEYSAVE                                                   
         BE    INVBPRD             PROD EQUIV ALREADY EXISTS                    
         SPACE                                                                  
* SEE IF A NEW PROD EQUIV ENTERED TWICE ON SCREEN                               
* SCRNPRD IS ONLY A LIST OF NEW PRODUCT EQUIVALENTS                             
         SPACE                                                                  
         LA    R4,SCRNPRD                                                       
         OC    8(3,R2),SPACES                                                   
VR14S    OC    0(3,R4),0(R4)       EMPTY SPOT?                                  
         BZ    VR14X                                                            
         CLC   0(3,R4),8(R2)       WAS IT ALREADY ENTERED?                      
         BNE   VR14T                                                            
         XC    SCRNPRD(45),SCRNPRD                                              
         B     INVBPRD                                                          
*                                                                               
VR14T    LA    R4,3(,R4)                                                        
         BCT   R1,VR14S                                                         
*                                                                               
VR14X    MVC   0(3,R4),8(R2)       PUT IT IN LIST                               
*                                                                               
         CLI   NOELEM,0                                                         
         BNE   VR15                                                             
         MVC   PEQPROD,8(R2)       JUST CHANGING ELEM                           
         OC    PEQPROD,SPACES                                                   
         MVI   DELPASPT,0          NOTHING WAS DELETED                          
         B     VR20                                                             
         SPACE                                                                  
         DROP  R6                                                               
         SPACE                                                                  
VR15     XC    ELEM,ELEM                                                        
         LA    R4,ELEM             NEW ELEMENT NEEDS TO BE ADDED                
         USING PEQDTAEL,R4                                                      
         MVI   PEQDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   PEQDTALN,PEQDTAX-PEQDTAEL                                        
         SPACE                                                                  
         MVI   PEQACT,C'A'         * FOR NOW, FORCE ALL ACTIVE *                
         SPACE                                                                  
         MVC   PEQPROD,8(R2)       MOVE IN NEW PROD EQUIV                       
         OC    PEQPROD,SPACES                                                   
         SPACE                                                                  
VR20     ZIC   R1,0(R2)            PT TO FLAG FIELD ON SCREEN                   
         AR    R2,R1                                                            
         SPACE                                                                  
* VALIDATE ACTIVE/INACTIVE/DELETE FIELD                                         
VR25     CLI   EQPRDFLG,0          WAS PROD EQUIV ENTERED?                      
         BNE   VR30                YES, CHECK FOR A/I/D                         
         CLI   5(R2),0             NO - ANY INPUT FOR A/I/D                     
         BE    VR50                NO - GO ON TO NEXT PAIR                      
         SR    R2,R1               YES (A/I) /NO PROD EQUIV                     
         B     MISSERR                                                          
         SPACE                                                                  
VR30     CLI   5(R2),0             ANY INPUT (A/I)                              
         B     VR40             BYPASS ACTIVE/INACTIVE FLAG                     
         SPACE 3                                                                
         BE    MISSERR             MUST BE ENTERED                              
         CLI   5(R2),1             ONLY 1 CHAR                                  
         BNE   TRAPERR                                                          
         CLI   8(R2),C'I'          IF INACTIVE                                  
         B     *+12                NO- CHECK ACTIVE                             
         MVI   PEQACT,C'I'                                                      
         B     VR40                                                             
         CLI   8(R2),C'A'          IF ACTIVE                                    
         BNE   INVALACT            NO- INVALID INPUT                            
         MVI   PEQACT,C'A'                                                      
*                                                                               
VR40     CLI   NOELEM,1            IS THIS BRAND NEW ELEMENT                    
         BNE   VR50                NO- DON'T ADD AGAIN                          
         SPACE                                                                  
         GOTO1 ADDELEM             YES- ADD IT                                  
         ZIC   RE,1(R6)            NEED TO POINT BEYOND                         
         AR    R6,RE               ELEMENT THAT WAS JUST ADDED                  
         B     VR50                                                             
*                                                                               
VR45     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
VR50     ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 NOW POINTS TO NUM FIELD                   
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 NOW POINTS TO EQUIV. PROD                 
         BCTR  R3,0                DECREMENT COUNTER                            
         LTR   R3,R3               HAVE WE USED ALL 15 FIELDS                   
         BZ    VR55                                                             
         MVI   EQPRDFLG,0          RESET FLAG                                   
         XC    SVPROD,SVPROD       RESET PROD EQUIV FOR DELETE                  
         CLI   NOELEM,1                                                         
         BE    VR10                                                             
         CLI   DELPASPT,1          DID WE JUST DELETE AN ELEM                   
         BNE   VR04                IF NOT DO NEXTEL                             
         CLI   0(R6),X'10'         YES- ALREADY -> TO NEXT ELEM                 
         BE    VR08                AND NEXT ELEM GOOD                           
         MVI   NOELEM,1            OR NO NEXT ELEM                              
         B     VR10                                                             
         SPACE                                                                  
VR55     MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY FOR GENCON                       
         B     DR                  NOW DISPLAY VALIDATED RECORD                 
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
* THIS PROCEDURE DELETES CURRENT ELEMENT /AND                                   
* ITS DELETES PASSIVE POINTER IF NECESSARY                                      
*                                                                               
DELPRDEQ DS    0H                                                               
         CLI   DELPASPT,1                                                       
         BNE   DELP10                                                           
         LR    R4,R6                                                            
         XC    DMCB(24),DMCB                                                    
         GOTO1 VRECUP,DMCB,(X'00',AIO1),(R4)                                    
DELP10   DS    0H                  SWITCH AIO'S FOR READING                     
         LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         MVC   0(2,R4),=XL2'0AB7'   RECORD TYPE                                 
         MVC   2(1,R4),BAGYMD                                                   
         MVC   3(2,R4),BCLT                                                     
         MVC   5(3,R4),SVPROD                                                   
         MVC   9(3,R4),QPRD                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DELP20                                                           
         BR    R5                                                               
DELP20   GOTO1 DATAMGR,DMCB,=C'DMREAD',SYSDIR,KEY,KEY                           
         OI    KEY+13,X'80'        DELETE IT                                    
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSDIR,KEY,KEY                            
         BR    R5                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 3                                                                
DR       GOTO1 CLRSCR,DMCB,(0,TRAPRDAH),TRATAGH                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     DR20                                                             
         USING PEQDTAEL,R6                                                      
         LA    R2,TRAPRDAH                                                      
         LA    R3,15               MAX PROD EQUIV FIELDS                        
         SPACE                                                                  
DR10     MVC   8(3,R2),PEQPROD                                                  
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 ->NUM FIELD NOW                           
*                                                                               
         CLI   PEQACT,0            ANYTHING THERE                               
         BE    DR15                                                             
         MVC   8(1,R2),PEQACT                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
DR15     ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 ->PROD EQUIV. FIELD NOW                   
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 ->NUM FIELD NOW                           
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         BCT   R3,DR10                                                          
*                                                                               
DR20     B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       LA    R2,TRAMEDH                                                       
         L     R4,AIO                                                           
         USING PEQKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         XC    WORK(L'TRACLT),WORK                                              
         GOTO1 CLUNPK,DMCB,PEQKCLT,WORK                                         
         CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         MVC   QCLT,WORK                                                        
         OC    QCLT,SPACES                                                      
         MVC   BCLT,PEQKCLT                                                     
         BAS   RE,FCLT                                                          
         SPACE                                                                  
DK10     XC    WORK(L'TRAPROD),WORK                                             
         MVC   WORK(L'PEQKPRD),PEQKPRD                                          
         CLC   TRAPROD,WORK                                                     
         BE    *+14                                                             
         MVC   TRAPROD,WORK         MOVE IN PROD                                
         OI    TRAPRODH+6,X'80'     SET ON TRANSMIT BIT                         
         MVC   QPRD,PEQKPRD        AFTER LIST PROD. NOT SAVED                   
         OC    QPRD,SPACES                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST                                                                   
         SPACE                                                                  
LR       OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GET THIS KEY                             
         SPACE                                                                  
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         SPACE                                                                  
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         SPACE                                                                  
         LA    R4,KEY              BUILD KEY FOR READHI                         
         USING PEQKEY,R4                                                        
         MVC   PEQKID,=X'0A37'                                                  
         MVC   PEQKAM,BAGYMD                                                    
         MVC   PEQKCLT,BCLT                                                     
         MVC   PEQKPRD,QPRD                                                     
         MVC   MYKEY,KEY           SAVE KEY FOR SEQ. COMPARE                    
         SPACE                                                                  
LR10     LA    R3,2                SAME RECORD TYPE AND A/M?                    
         CLI   BCLT,0              WAS CLIENT IN KEY?                           
         BE    LR25                NO-LIS SPECIF MED & ALL CLIENT/PROD          
*                                                                               
*                                  CLIENT ENTERED - CHECK PROD                  
         LA    R3,4                SO FAR- LIST SPEC. MED/CLIENT                
         CLI   BPRD,0              ANY PRODUCT IN KEY?                          
         BE    LR25                NO -LIS SPEC MED/CLIENT & ALL PROD           
*                                                                               
*                                  PROD IN KEY LIS SPECIF MED/CLI/PROD          
         LA    R3,6                                                             
LR25     GOTO1 HIGH                                                             
         EX    R3,LRCLC                                                         
         BNE   EXIT                                                             
         B     LR30                                                             
         SPACE                                                                  
LRCLC    CLC   KEY(0),MYKEY                                                     
         SPACE                                                                  
LR30     DS    0H                                                               
         OC    BCLT,BCLT           WAS CLIENT ENTERED ?                         
         BNZ   LR35                 YES                                         
         SPACE                                                                  
         LA    R4,KEY                                                           
         SPACE                                                                  
         MVC   BCLT,PEQKCLT                                                     
         BAS   RE,FCLT                                                          
         BE    LR35                                                             
         SPACE                                                                  
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVI   PEQKPRD,X'FF'       GET NEXT CLIENT                              
         B     LR25                                                             
         SPACE                                                                  
LR35     L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         LR    R6,R4               MOVE IN INFO FOR LIST                        
         GOTO1 GETREC                                                           
         SPACE                                                                  
         USING PEQKEY,R4                                                        
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LRR                                                              
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         GOTO1 CLUNPK,DMCB,PEQKCLT,WORK                                         
         MVC   LCLT,WORK           CLIENT NAME                                  
         MVC   LPROD,PEQKPRD       PRODUCT                                      
         SPACE                                                                  
         MVI   ELCODE,X'10'        GET PROD. EQUIVALENTS                        
         BAS   RE,GETEL                                                         
         BNE   LR50                                                             
         USING PEQDTAEL,R6                                                      
         LA    R1,15               FIT ON LIST LINE                             
         LA    R5,LPRDEQ                                                        
*                                                                               
LR40     MVC   0(3,R5),PEQPROD     MOVE IN PROD. EQUIV                          
*        MVI   3(R5),C'-'                                                       
*        MVC   4(1,R5),PEQACT                                                   
*                                                                               
         BCTR  R1,0                                                             
         LTR   R1,R1               HAVE WE MOVED ALL PROD EQUIV                 
         BZ    LR45                                                             
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LR50                                                             
         MVI   3(R5),C','          SEPARATE PROD. EQUIV'S                       
         LA    R5,4(,R5)                                                        
         B     LR40                                                             
*                                                                               
LR45     MVC   3(5,R5),=C',MORE'                                                
*                                                                               
LR50     GOTO1 LISTMON                                                          
         GOTO1 SEQ                 DO READ SEQUENTIAL                           
         EX    R3,LRCLC                                                         
         BE    LR30                                                             
         B     EXIT                                                             
         EJECT                                                                  
LRR      MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,PEQKCLT,PCLT                                         
         SPACE                                                                  
         L     R1,AIO2                                                          
         CLC   1(1,R1),BAGYMD                                                   
         BNE   LRR10                                                            
         CLC   2(2,R1),PEQKCLT                                                  
         BE    LRR14                                                            
LRR10    XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),PEQKCLT                                                 
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   CLTNM,CNAME-CLTHDR(R1)                                           
         SPACE                                                                  
LRR14    MVC   PCLTNM,CLTNM                                                     
         SPACE                                                                  
         MVC   PPROD,PEQKPRD       PRODUCT                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),PEQKCLT                                                 
         MVC   KEY+4(3),PEQKPRD    PRODUCT                                      
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   PPRODNM,PNAME-PRDHDR(R5)                                         
         SPACE                                                                  
         MVI   ELCODE,X'10'        GET PROD. EQUIVALENTS                        
         BAS   RE,GETEL                                                         
         BE    LRR40                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR50                                                            
         USING PEQDTAEL,R6                                                      
         SPACE                                                                  
LRR40    MVC   PEPROD,PEQPROD      MOVE IN PROD. EQUIV                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),PEQKCLT                                                 
         MVC   KEY+4(3),PEQPROD    EQUIVALENT PRODUCT                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   PEPRODNM,PNAME-PRDHDR(R5)                                        
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BE    LRR40                                                            
         SPACE                                                                  
LRR50    GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 DO READ SEQUENTIAL                           
         EX    R3,LRCLC                                                         
         BE    LR30                                                             
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
FCLT     NTR1                                                                   
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
FCLT20   DS    0H                                                               
         XC    BCLT,BCLT           VALICLT FILLS IN BCLT                        
         SPACE                                                                  
         L     R0,AIO1             MOVE RECORD BACK                             
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
         CLI   ERROR,0             SET CC FOR RETURN                            
         B     EXIT                                                             
         EJECT                                                                  
* DELETE PASSIVE POINTERS                                                       
         SPACE                                                                  
XDEL     DS    0H                                                               
         L     R6,AIO                                                           
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         XC    KEY(13),KEY                                                      
         MVC   PEQPID,=XL2'0AB7'                                                
         MVC   PEQPAM,2(R6)                                                     
         MVC   PEQPCLT,3(R6)                                                    
         MVC   PEQPBPRD,5(R6)                                                   
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
XDEL10   BNE   EXIT                                                             
         USING PEQDTAEL,R6                                                      
         MVC   PEQPEPRD,PEQPROD                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',SYSDIR,KEY,KEY                           
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSDIR,KEY,KEY                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     XDEL10                                                           
         SPACE 5                                                                
* RESTORE PASSIVE POINTERS                                                      
XREST    DS    0H                                                               
         L     R6,AIO                                                           
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         XC    KEY(13),KEY                                                      
         MVC   PEQPID,=XL2'0AB7'                                                
         MVC   PEQPAM,2(R6)                                                     
         MVC   PEQPCLT,3(R6)                                                    
         MVC   PEQPBPRD,5(R6)                                                   
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
XREST10  BNE   EXIT                                                             
         USING PEQDTAEL,R6                                                      
         MVC   PEQPEPRD,PEQPROD                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',SYSDIR,KEY,KEY                           
         NI    KEY+13,X'00'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSDIR,KEY,KEY                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     XREST10                                                          
         SPACE 2                                                                
* READS THROUGH RECORD IN AIO AND ADDS PASSIVE                                  
* POINTERS WHERE NEEDED                                                         
*                                                                               
XADD     DS    0H                                                               
         L     R6,AIO                                                           
         CLI   MODE,XRECADD                                                     
         BNE   XADD05                                                           
         MVC   SVDA,KEY+0          ADD   - D/A                                  
         B     *+10                                                             
XADD05   MVC   SVDA,KEY+14         THIS IS A CHANGE                             
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQPID,=XL2'0AB7'                                                
         MVC   PEQPAM,2(R6)                                                     
         MVC   PEQPCLT,3(R6)                                                    
         MVC   PEQPBPRD,5(R6)                                                   
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
XADD10   BNE   EXIT                                                             
         USING PEQDTAEL,R6                                                      
         MVC   PEQPEPRD,PEQPROD                                                 
         MVC   AIO,AIO2                                                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),SYSDIR,KEYSAVE,KEY               
         CLC   KEY(11),KEYSAVE                                                  
         BE    XADD20              PASSIVE PTR ALREADY THERE                    
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+14(4),SVDA                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
         CLI   DMCB+8,0                                                         
         BE    XADD30              GO TO NEXT                                   
         DC    H'0'                                                             
*                                                                               
XADD20   CLI   KEY+13,X'80'        WAS IT DELETED?                              
         BNE   XADD30              NO- GET NEXT ONE                             
         NI    KEY+13,X'00'        UNDELETE IT                                  
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSDIR,KEY,KEY                            
*                                                                               
XADD30   MVC   AIO,AIO1                                                         
         BAS   RE,NEXTEL                                                        
         B     XADD10                                                           
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 2                                                                
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H6+10(L'QMED),QMED                                               
         MVC   H6+15(L'MEDNM),MEDNM                                             
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 3                                                                
* CLEAR DISPLAY AREA OF SCREEN *                                                
* CLRSCR  - CLEAR AND FOUT FIELDS                                               
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
CLRSCR   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLRSCR2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    4(R2),X'20'         SET VALIDITY BIT                             
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLRSCR4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLRSCR2             NO-CONTINUE                                  
         B     EXIT                YES-ALL DONE                                 
         SPACE 1                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLRSCR4                                                        
         EJECT                                                                  
INVBPRD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(38),=C'THIS EQUIVALENT PRODUCT ALREADY EXISTSC        
               '                                                                
         B     ERREXIT                                                          
INVEPRD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(30),=C'THIS PRODUCT IS A BASE PRODUCT'                
         B     ERREXIT                                                          
INVPRD1  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(42),=C'PRODUCT CANNOT BE BASE -ALREADY EQUIVAX        
               LENT'                                                            
         B     ERREXIT                                                          
NOPRD    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(27),=C'THIS PRODUCT DOES NOT EXIST'                   
         B     ERREXIT                                                          
INVNONE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(30),=C'NONE ONLY VALID IN FIRST FIELD'                
         B     ERREXIT                                                          
INVALACT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(17),=C'VALID INPUT - A  '                             
         SPACE                                                                  
ERREXIT  DS    0H                                                               
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         GOTO1 ERREX2                                                           
         DC    H'0'                                                             
INVPRDER MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'--------------'                                           
         SSPEC H5,3,PAGE                                                        
         SSPEC H6,3,C'MEDIA'                                                    
         SSPEC H1,30,C'PRODUCT EQUIVALENT LIST'                                 
         SSPEC H2,30,C'-----------------------'                                 
         SSPEC H1,60,AGYNAME                                                    
         SSPEC H2,60,AGYADD                                                     
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,72,RUN                                                        
         SSPEC H5,60,REQUESTOR                                                  
         SSPEC H8,3,C'CLIENT'                                                   
         SSPEC H9,3,C'------------------------'                                 
         SSPEC H8,33,C'PRODUCT'                                                 
         SSPEC H9,33,C'-----------------------'                                 
         SSPEC H8,63,C'EQUIVALENT PRODUCTS'                                     
         SSPEC H9,63,C'---------------------- '                                 
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA84D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
RELO     DS    F                                                                
EQPRDFLG DS    XL1                                                              
DELPASPT DS    XL1                                                              
NOELEM   DS    XL1                                                              
SVDA     DS    XL4                                                              
SVPROD   DS    CL3                                                              
SVBCLT   DS    XL2                                                              
BBCLT    DS    XL2                                                              
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
MYKEY    DS    CL32                                                             
SCRNPRD  DS    15CL3                                                            
         SPACE                                                                  
* ONLINE LIST LINE                                                              
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPROD    DS    CL3                                                              
         DS    CL2                                                              
LPRDEQ   DS    CL64                                                             
         SPACE                                                                  
* REPORT PRINT LINE                                                             
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PCLTNM   DS    CL20                                                             
         DS    CL5                                                              
PPROD    DS    CL3                                                              
         DS    CL2                                                              
PPRODNM  DS    CL20                                                             
         DS    CL5                                                              
PEPROD   DS    CL3                                                              
         DS    CL2                                                              
PEPRODNM DS    CL20                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPTRA64   03/06/07'                                      
         END                                                                    
