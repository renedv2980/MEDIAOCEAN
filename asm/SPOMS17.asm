*          DATA SET SPOMS17    AT LEVEL 036 AS OF 01/16/07                      
*PHASE T23417A                                                                  
T23417   TITLE 'SPOMS17 - DESTOV/MAINT'                                         
T23417   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23417*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'                                                       
         GOTO1 INITIAL,DMCB,SPFTABLE  INITIALIZE THE PFKEYS                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*======================================================================         
* VALIDATE THE KEY                                                              
*======================================================================         
         SPACE 1                                                                
VK       XC    SAVEKEY,SAVEKEY                                                  
         MVI   MISCFLG1,0                                                       
         MVI   MISCFLG2,0                                                       
         LA    R2,DOVMEDH          VALIDATE MEDIA                               
         TM    4(R2),X'20'          PREV VALIDATED?                             
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'          PREV VALIDATED                              
*                                                                               
         MVC   DOVDEF+8(4),=C' EDI'                                             
         NI    DOVINVH+1,X'FF'-X'0C'                                            
         MVC   BYTE,BAGYMD                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'02'                                                       
         BNE   VKMEDX                                                           
         OI    MISCFLG2,MF2RADIO                                                
         MVC   DOVDEF+8(4),=C'REDI'                                             
         OI    DOVINVH+1,X'0C'                                                  
VKMEDX   OI    DOVDEFH+6,X'80'         TRANSMIT                                 
*                                                                               
VKBUYR   LA    R2,DOVBYRH                                                       
         TM    4(R2),X'20'          PREV VALIDATED?                             
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIBUYR,DMCB,8(R2)                                              
         OI    4(R2),X'20'          PREV VALIDATED                              
*                                                                               
         LA    R2,DOVSTAH          VALIDATE STATION                             
         TM    4(R2),X'20'          PREV VALIDATED?                             
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
         CLI   5(R2),0                                                          
         BE    MISSFLD             MISSING INPUT                                
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'          PREV VALIDATED                              
*                                                                               
         LA    R2,DOVCLTH          VALIDATE CLIENT                              
         TM    4(R2),X'20'          PREV VALIDATED?                             
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'          PREV VALIDATED                              
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0OM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
         CLI   CLTOFFCE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFCE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,PROFOM,DATAMGR                                 
         CLI   QMED,C'R'           TV DOESN'T CARE ABOUT MF2FXRAD               
         BNE   VK00                                                             
         CLI   POMFXRAD,C'N'       ONLY FAX RADIO ORDERS?                       
         BE    *+8                 NO                                           
         OI    MISCFLG2,MF2FXRAD   YES!!                                        
*                                                                               
VK00     DS    0H                                                               
*&&DO                                                                           
VK50     LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING GBDEKEY,R6           BDE E-MAIL IDENTIFIER                       
         MVI   GBDEID,GBDEIDQ       C'B'                                        
         MVC   GBDEORG,BDEGROUP     ORGANISATION                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                   
         CLC   KEY(GBDENAME-GBDEKEY),KEYSAVE                                    
         BNE   VK05                                                             
         GOTO1 (RF),(R1),=C'GETREC',=C'GENFIL',KEY+36,AIO,DMWORK                
         L     R6,AIO                                                           
         LA    R6,GBDRFST(R6)       FULL NAME ELEMENT                           
         DROP  R6                                                               
*                                                                               
         USING GBNELD,R6                                                        
         MVC   DOVCOM,GBNORG        MOVE ORGANISATION TO COMPANY                
         OI    DOVCOMH+6,X'80'      XMIT                                        
         DROP  R6                                                               
*&&                                                                             
VK05     OI    DOVORDH+1,X'20'     PROTECT THE FIELD                            
         OI    DOVDSTH+1,X'20'     PROTECT THE FIELD                            
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL?                           
         BE    *+12                                                             
         OI    DOVORDH+1,X'0C'                                                  
         OI    DOVDSTH+1,X'0C'                                                  
*                                                                               
         OI    DOVINVH+1,X'0C'     REMOVE THE EMAIL INVITE LINE                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DMTHKEY,R4                                                       
         MVI   DMTHTYPE,DMTHTYPQ  TYPE                                          
         MVI   DMTHSBTY,DMTHSBTQ  SUB-TYPE                                      
         MVC   DMTHAGMD,BAGYMD                                                  
         MVC   DMTHBYR,QBYR                                                     
         MVC   DMTHSTA,BMKTSTA+2                                                
         MVC   DMTHCLT,BCLT                                                     
*                                                                               
*        XC    DOVSRC+9(16),DOVSRC+9                                            
*                                                                               
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     FIND REC FOR THIS CLT ?                      
         BNE   VK10                                                             
*        MVC   DOVSRC+9(6),=C'CLIENT'                                           
*        MVC   DOVSRC+16(3),DOVCLT                                              
         MVC   SAVEKEY,KEY          SAVE ACTUAL KEY USED                        
         B     VKX                                                              
*                                                                               
VK10     DS    0H                                                               
         MVC   KEY,KEYSAVE         TRY FOR ALL CLIENT RECORD                    
         XC    DMTHCLT,DMTHCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VK20                                                             
*        XC    DOVSRC+9(16),DOVSRC+9                                            
*        MVC   DOVSRC+9(11),=C'ALL CLIENTS'                                     
         MVC   SAVEKEY,KEY          SAVE ACTUAL KEY USED                        
         B     VKX                                                              
*                                                                               
VK20     MVC   KEY,KEYSAVE         SO WE CAN PASS GENCON                        
         MVC   DMTHCLT,BCLT                                                     
*                                                                               
VKX      NI    DMINBTS,X'FF'-X'08'                                              
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*======================================================================         
* DISPLAY THE RECORD                                                            
*======================================================================         
         SPACE 1                                                                
DR       DS    0H                                                               
         SR    RE,RE               TWAXC DOVSELH,DOVSLDFH,PROT=Y                
         LA    R1,DOVSELH                                                       
         LA    RF,DOVSLDFH                                                      
DR02     IC    RE,0(R1)                                                         
         SHI   RE,9                                                             
         LTR   RE,RE                                                            
         BM    DR09                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    4(R2),X'20'                                                      
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,DR02                                                       
*                                                                               
DR09     OI    DOVSLEMH+4,X'20'                                                 
         OI    DOVSLEMH+6,X'80'                                                 
         OI    DOVSLFXH+4,X'20'                                                 
         OI    DOVSLFXH+6,X'80'                                                 
         OI    DOVFOR2H+6,X'80'                                                 
         OI    DOVSLOTH+4,X'20'                                                 
         OI    DOVSLOTH+6,X'80'                                                 
         OI    DOVFXNOH+4,X'20'                                                 
         OI    DOVFXNOH+6,X'80'                                                 
         MVC   ELEM(11),DOVDSTH                                                 
*                                                                               
         LA    R2,DOVORDH                                                       
*                                                                               
         BAS   RE,VALORDR                                                       
         BNE   DR09A                                                            
*                                                                               
         LA    R2,KEY                                                           
         USING DOKEY,R2                                                         
         XC    KEY,KEY                                                          
         MVI   DOKTYPE,DOKTYPQ     X'0D'                                        
         MVI   DOKSUBTY,DOKSTYPQ   X'34'                                        
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORD                                                  
         DROP  R2                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE  DID I GET MY KEY?                     
         BNE   VRXXIT                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,PVCNFM           CHK ORDER IS PREVIOUSLY SENT/CNFMD           
*                                  AND TURN BITS ON IN MISCFLG2                 
DR09A    XC    BLOCK,BLOCK           FAKE OUT ROUTE FIELD                       
         MVC   BLOCK(8),DOVSTAH                                                 
         MVI   BLOCK,8+64                                                       
         OI    BLOCK+4,X'20'                                                    
         GOTO1 RD4ROUTE,DMCB,(X'A0',ELEM),BLOCK,(MISCFLG2,0),0                  
         BE    DR09C                                                            
DR09INV  NI    DOVSTAH+4,X'FF'-X'20'        TURN OFF VALID BIT                  
         B     ERREXIT                                                          
*                                                                               
DR09C    OI    DOVDEFH+1,X'0C'       BLANK OUT "DEFAULT..." LINE                
*                                                                               
         CLI   DOVORD,C'O'         FAXING OOW TO MO?                            
         BE    DR09F                                                            
*                                                                               
         OC    BINORD,BINORD                                                    
         BZ    DR09D                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         USING DOSPELD,R6                                                       
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
         TM    DOSPFLG1,DOSPAVN                                                 
         BO    DR09F                                                            
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
         TM    DOSPFLG1,DOSPEML                                                 
         BNZ   DR09F                                                            
         DROP  R6                                                               
*                                                                               
DR09D    TM    MISCFLG2,MF2RADIO+MF2FXRAD  FAX ONLY RADIO?                      
         BO    DR09F                       YES, DON'T OFFER INBOX               
*                                                                               
         TM    MISCFLG2,MF2NOINB   PRV CONFIRMED OR NOTDARED?                   
         BO    DR09F                 YES, DON'T OFFER INBOX                     
*                                                                               
         CLC   =C'FAX: ',BLOCK+8                                                
         BE    DR09F                                                            
         CLC   =C'EML: ',BLOCK+8                                                
         BE    DR09F                                                            
         OI    MISCFLG1,MF1INBOX     THE DEFAULT IS INBOX                       
         OI    DOVSLDFH+4,X'20'      THIS FIELD SHOULDN'T EXIST IF...           
         OI    DOVSLDFH+6,X'80'      ...THERE IS NO INBOX  (SEE DR09C)          
         NI    DOVDEFH+1,X'FF'-X'0C'   BRING IT BACK, IT'S INBOX                
*                                                                               
DR09F    XC    BLOCK,BLOCK           FAKE OUT ROUTE FIELD                       
         MVC   BLOCK(8),DOVSTAH                                                 
         MVI   BLOCK,8+64                                                       
         OI    BLOCK+4,X'20'                                                    
*                                                                               
         TM    MISCFLG2,MF2PRSNT     PREVIOUSLY SENT?                           
         BZ    DR09G                 NO                                         
         BRAS  RE,FAKELMRC           YES, USE FAKE LASTMETHOD REC               
         GOTO1 RD4ROUTE,DMCB,(X'60',ELEM),BLOCK,(MISCFLG2,0),0                  
         B     DR09H                                                            
*                                                                               
DR09G    GOTO1 RD4ROUTE,DMCB,(X'20',ELEM),BLOCK,(MISCFLG2,0),0                  
         BNE   DR09INV                                                          
*                                                                               
DR09H    MVC   SAVEDEST,ELEM+8                                                  
*                                                                               
         CLI   SAVEDEST,0                                                       
         BNE   *+8                                                              
         MVI   SAVEDEST,C'R'                                                    
*                                                                               
         MVI   SAVEMTHD,C'F'                                                    
         CLC   =C'FAX: ',BLOCK+8                                                
         BE    DR09X                                                            
         MVI   SAVEMTHD,C'E'                                                    
         CLC   =C'EML: ',BLOCK+8                                                
         BE    DR09X                                                            
         MVI   SAVEMTHD,C'I'                                                    
*                                                                               
DR09X    LA    R2,DOVSELH                                                       
         USING LIND,R2                                                          
*&&DO                                                                           
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING GBDEKEY,R6                                                       
         MVI   GBDEID,GBDEIDQ                                                   
         MVC   GBDEORG,BDEGROUP                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                   
         B     DR15                                                             
DR10     GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEYSAVE,KEY                   
DR15     CLC   KEY(GBDENAME-GBDEKEY),KEYSAVE                                    
         BNE   DR50                                                             
         GOTO1 (RF),(R1),=C'GETREC',=C'GENFIL',KEY+36,AIO,DMWORK                
         L     R6,AIO                                                           
         LA    R6,GBDRFST(R6)                                                   
         USING GBNELD,R6                                                        
         MVC   LINROUT(L'GBNFNM),GBNFNM                                         
         OI    LINROUTH+6,X'80'                                                 
         LA    R2,LINNEXTL                                                      
         LA    R0,DOVSLEMH                                                      
         CR    R2,R0                                                            
         BL    DR10                                                             
         DROP  R6                                                               
*&&                                                                             
***************                                                                 
* NOW FOR THE FAX NUMBER                                                        
***************                                                                 
DR50     TM    MISCFLG2,MF2RADIO   RADIO ORDER?                                 
         BZ    DR51                TV USES DARADM ONLY FOR REP                  
         TM    MISCFLG2,MF2FXRAD   REDI?                                        
         BNZ   DR53                NO, FAX ONLY, SO DESTINE CAN BE REP          
*                                                                               
DR51     CLI   SAVEDEST,C'R'       TV AND REDI/REP?                             
         BE    DR90                USE DARADM RECORD FOR REP FAX #              
*                                                                               
DR53     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DSRKEY,R4                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D3D'                                      
         MVI   DSRKSBTY,DSRKSBTQ                                                
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
         MVC   DSRKCLT,BCLT         CLIENT LEVEL?                               
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE                                            
         BE    DR60                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DSRKEY,R6                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D'                                        
         MVI   DSRKSBTY,DSRKSBTQ   X'3D'                                        
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
         MVI   DSRKCLT,C'*'        OFFICE                                       
         MVC   DSRKCLT+1(1),CLTOFFCE                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE                                            
         BE    DR60                                                             
*                                                                               
         CLI   PFFXUOFL,C'Y'       USES OFFICE LIST?                            
         BNE   DR55                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DSRKEY,R6                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D'                                        
         MVI   DSRKSBTY,DSRKSBTQ   X'3D'                                        
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
         MVI   DSRKCLT,C'$'        OFFICE LIST                                  
         MVC   DSRKCLT+1(1),PFFXOLCD   OFFICE LIST CODE FROM PROFILE            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE                                            
         BE    DR60                                                             
*                                                                               
DR55     XC    KEY,KEY              BY STATION CALL LETTERS ONLY                
         LA    R6,KEY                                                           
         USING DSRKEY,R6                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D'                                        
         MVI   DSRKSBTY,DSRKSBTQ   X'3D'                                        
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE                                            
         BNE   DR100                                                            
*                                                                               
DR60     GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSRDRELQ     X'20' - RADIO DESTINATION ROUTE ELEM         
         BAS   RE,GETEL            DID WE FIND IT?                              
         BNE   DR75                NO, THEN TV, JUST DISPLAY                    
         USING DSRDRELD,R6                                                      
*                                                                               
         TM    MISCFLG2,MF2FXRAD   RADIO FAXING ALL ORDERS                      
         BO    DR70                YES                                          
         CLI   DSRDRDES,C'R'       NO, IS DESTINE NUMBER FOR STA?               
         BE    DR100                   NO, DON'T BOTHER SHOWING IT              
*                                                                               
DR70     CLC   DSRDRDES,SAVEDEST   SAME DESTINATION?                            
         BNE   DR100               NO                                           
*                                  YES, DISPLAY IT                              
DR75     L     R6,AIO                                                           
         USING DSRKEY,R6                                                        
         LA    R6,DSRFRST                                                       
         USING DSRIDELD,R6                                                      
         MVC   DOVFOR2(L'DSRIDFAX),DSRIDFAX                                     
         XC    SAVEFAX,SAVEFAX                                                  
         MVC   SAVEFAX(L'DSRIDFAX),DSRIDFAX                                     
         B     DR100                                                            
         DROP  R6                                                               
***************                                                                 
* REP FAXING                                                                    
***************                                                                 
DR90     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTFXREC,R6                                                       
         MVI   CTFXKTYP,CTFXEQU    X'09'                                        
         MVC   CTFXAGY,=C'D7'      DARADM  ID FOR DARE FAX NUMBERS              
         MVC   CTFXCODE,SVREP+15   FAX CODE                                     
         OC    CTFXCODE,=7X'40'    BLANK FILL                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO                       
         L     R6,AIO                                                           
         CLC   KEY(25),0(R6)       SAME FAX ID?                                 
         BNE   DR100               NO DARADM RECORD FOR THIS REP                
*                                                                               
         LA    R6,CTFXEL1                                                       
         CLI   0(R6),CTFX1ELQ                                                   
         BNE   DR100                                                            
         USING CTFX1EL,R6                                                       
         ZIC   R1,CTFX1LEN                                                      
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     DR99                                                             
         MVC   DOVFOR2(0),CTFX1NUM                                              
DR99     EX    R1,*+8                                                           
         B     DR100                                                            
         MVC   SAVEFAX(0),CTFX1NUM                                              
         DROP  R6                                                               
*                                                                               
DR100    OC    SAVEKEY,SAVEKEY     LOOK IN LAST METHOD RECORD                   
         BZ    DRX                 IF ANY                                       
         XC    KEY,KEY             FOR TEMPORARY FAX NUMBER                     
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DMFXELQ      X'10' - TEMPORARY FAX OVERRIDE               
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING DMFXELD,R6                                                       
*                                                                               
         CLC   DMFXDATE,JDTTODAY   OVERRIDE NUMBER ONLY VALID TODAY             
         BNE   *+10                NOT VALID                                    
         MVC   DOVFXNO,DMFXOVRD    VALID, SHOW IT                               
         DROP  R6                                                               
*                                                                               
DRX      B     EXIT                                                             
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       TM    MISCFLG1,MF1KYCHG                                                
         BNZ   DR                                                               
         NI    MISCFLG1,X'FF'-MF1DOV   CLEAR OUT MF1DOV BIT                     
         TM    DOVDEFH+1,X'0C'     IS IT ZERO INTENSITY?                        
         BO    VRCHECK1             YES, SKIP TO NEXT CHECK                     
*                                                                               
***  SEE IF MORE THAN 1 OPTION IS SELECTED     MHC  08/13/02                    
VRCHECK  LA    R2,DOVSLDFH         (DEFAULT TO INBOX OPTION)                    
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BO    VRCHECK1             YEAH, DIDN'T PICK THIS                      
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+12                 YEAH, CONTINUE                              
         OI    4(R2),X'20'         IT'S OK AGAIN                                
         B     VRCHECK1                                                         
*                                                                               
         OI    MISCFLG1,MF1DOV     DEFAULT TO INBOX SELECTED                    
*                                                                               
VRCHECK1 TM    DOVINVH+1,X'0C'     IS IT ZERO INTENSITY?                        
         BO    VRCHECK2             YES, SKIP TO NEXT CHECK                     
         LA    R2,DOVSLEMH         (FAX AND INVITE OPTION)                      
         TM    4(R2),X'20'                                                      
         BO    VRCHECK2                                                         
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+12                 YEAH, CONTINUE                              
         OI    4(R2),X'20'         IT'S OK AGAIN                                
         B     VRCHECK2                                                         
*                                                                               
         TM    MISCFLG1,MF1DOV     MORE THAN 1 SELECTED?                        
         BZ    *+8                  NOPE, CONTINUE                              
         B     OOPTION             ERROR, MORE THAN 1 OPTION PICKED             
         OI    MISCFLG1,MF1DOV     FAX AND INVITE SELECTED                      
*                                                                               
VRCHECK2 LA    R2,DOVSLFXH         (FAX ORDER TO OPTION)                        
         TM    4(R2),X'20'                                                      
         BO    VRCHECK3                                                         
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+12                 YEAH, CONTINUE                              
         OI    4(R2),X'20'         IT'S OK AGAIN                                
         B     VRCHECK3                                                         
*                                                                               
         TM    MISCFLG1,MF1DOV                                                  
         BZ    *+8                                                              
         B     OOPTION             ERROR, MORE THAN 1 OPTION PICKED             
         OI    MISCFLG1,MF1DOV     FAX ORDER TO SELECTED                        
*                                                                               
VRCHECK3 LA    R2,DOVSLOTH         (FAX ORDER TO OPTION)                        
         TM    4(R2),X'20'                                                      
         BO    VR09                                                             
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+12                 YEAH, CONTINUE                              
         OI    4(R2),X'20'         IT'S OK AGAIN                                
         B     VR09                                                             
*                                                                               
         TM    MISCFLG1,MF1DOV                                                  
         BZ    *+8                                                              
         B     OOPTION             ERROR, MORE THAN 1 OPTION PICKED             
         OI    MISCFLG1,MF1DOV     FAX OVERRIDE TO SELECTED                     
***     CHECK FOR MULTIOPTION SELECT DONE                                       
*                                                                               
***************                                                                 
* GO THROUGH THE BDE LINES                                                      
***************                                                                 
VR09     LA    R2,DOVSELH                                                       
         USING LIND,R2                                                          
VR10     TM    LINSELH+4,X'20'     SELECTED THIS LINE?                          
         BZ    VR20                YES                                          
VR15     LA    R2,LINNEXTL                                                      
         LA    R0,DOVSLDFH                                                      
         CR    R2,R0                                                            
         BNL   VR30                NONE SELECTED, CHECK FAX LINES               
         B     VR10                                                             
***************                                                                 
* SELECTED A BDE LINE                                                           
***************                                                                 
VR20     CLC   LINSEL,SPACES       NOTHING HERE?                                
         BNH   *+14                                                             
         CLC   LINROUT,SPACES                                                   
         BH    VR23                                                             
         XC    LINSEL,LINSEL       NOTHING, CLEAR THIS SELECT FIELD             
         OI    LINSELH+4,X'20'                                                  
         OI    LINSELH+6,X'80'                                                  
         B     VR15                                                             
*                                                                               
VR23     MVI   SAVEMTHD,C'E'                                                    
         B     VR100                                                            
***************                                                                 
* NEW "DEFAULT TO INBOX" OPTION        MHC  08/13/02                            
***************                                                                 
VR30     LA    R2,DOVSLDFH                                                      
         TM    DOVDEFH+1,X'0C'     IS IT ZERO INTENSITY?                        
         BNO   VR35                 NO, CONTINUE THIS                           
*                                                                               
         XC    8(3,R2),8(R2)       SOMETHING IS HERE....                        
         OI    4(R2),X'20'         ....BETTER CLEAR IT FIRST                    
         OI    6(R2),X'80'                                                      
         B     VR40                MOVE ON TO NEXT PART                         
*                                                                               
VR35     TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   VR40                                                             
         MVI   SAVEMTHD,C'I'                                                    
         B     VR100                                                            
***************                                                                 
* GO THROUGH THE FAX LINES                                                      
***************                                                                 
VR40     LA    R2,DOVSLEMH                                                      
         TM    DOVINVH+1,X'0C'     IS IT ZERO INTENSITY?                        
         BNO   VR45                 NO, CONTINUE THIS                           
*                                                                               
         XC    8(3,R2),8(R2)       SOMETHING IS HERE....                        
         OI    4(R2),X'20'         ....BETTER CLEAR IT FIRST                    
         OI    6(R2),X'80'                                                      
         B     VR60                MOVE ON TO NEXT PART                         
*                                                                               
VR45     TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   VR60                                                             
         CLC   DOVSLEM,SPACES                                                   
         BH    VR50                                                             
         XC    8(3,R2),8(R2)       NOTHING, CLEAR THIS SELECT FIELD             
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         B     VR60                                                             
*                                                                               
VR50     OI    MISCFLG1,MF1INVIT                                                
         MVI   SAVEMTHD,C'F'                                                    
         B     VR100                                                            
***************                                                                 
* SELECT TO SEND VIA FAX AND KEEP IT AS FAX                                     
***************                                                                 
VR60     LA    R2,DOVSLFXH                                                      
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   VR80                                                             
         CLC   DOVSLFX,SPACES                                                   
         BH    VR70                                                             
         XC    8(3,R2),8(R2)       NOTHING, CLEAR THIS SELECT FIELD             
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         B     VR80                                                             
*                                                                               
VR70     CLC   DOVFOR2(16),SPACES                                               
         BNH   INVLFLD                                                          
*                                                                               
         MVI   SAVEMTHD,C'F'                                                    
         B     VR100                                                            
***************                                                                 
* SELECT TO SEND VIA FAX OVERRIDE AND KEEP IT AS FAX OVERRIDE                   
***************                                                                 
VR80     LA    R2,DOVSLOTH                                                      
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   VRX                                                              
         CLC   DOVSLOT,SPACES                                                   
         BH    VR90                                                             
         XC    8(3,R2),8(R2)       NOTHING, CLEAR THIS SELECT FIELD             
         B     VRX                                                              
*                                                                               
VR90     CLC   =C'OTHER NUMBER',DOVFXNO                                         
         BE    INVLFLD                                                          
*                                                                               
         OI    MISCFLG1,MF1OVRDE                                                
         MVI   SAVEMTHD,C'F'                                                    
***************                                                                 
* UPDATE THE LAST METHOD RECORD                                                 
***************                                                                 
VR100    DS    0H                                                               
         OC    SAVEKEY,SAVEKEY     DID WE FIND A LAST METHOD BEFORE?            
         BZ    VR105                                                            
         BRAS  RE,RDDSVKEY        READ FOR UPDATE LAST METHOD KEY               
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         B     VR110                                                            
*                                                                               
VR105    L     RE,AIO              CLEAR AIO                                    
         LHI   RF,LIOS                                                          
         XCEFL                                                                  
*                                                                               
         L     R6,AIO                                                           
         USING DMTHKEY,R6                                                       
         MVI   DMTHTYPE,DMTHTYPQ  TYPE                                          
         MVI   DMTHSBTY,DMTHSBTQ  SUB-TYPE                                      
         MVC   DMTHAGMD,BAGYMD                                                  
         MVC   DMTHBYR,QBYR                                                     
         MVC   DMTHSTA,BMKTSTA+2                                                
         MVC   DMTHCLT,BCLT                                                     
         MVC   DMTHAGY,AGENCY                                                   
         LA    R1,DMTHLNQ+DMTHFRST-DMTHKEY+1  L(L'REC+L'DMTHLNQ)                
         STCM  R1,3,DMTHLEN            L(REC) W/O THE ACTVD                     
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         USING DMTHELD,R6                                                       
         MVI   DMTHEL,DMTHELQ      SETUP THE METHOD ELEMENT                     
         MVI   DMTHELLN,DMTHLNQ                                                 
VR110    MVC   DMTHDEST,SAVEDEST                                                
         MVC   DMTHMTHD,SAVEMTHD                                                
*                                                                               
         NI    DMTHFLG1,X'FF'-DMF1KFAX  DON'T NEED THIS IF NOT FAX              
         CLI   SAVEMTHD,C'F'       AM I FAX?                                    
         BNE   VR130                                                            
         OI    DMTHFLG1,DMF1KFAX                                                
*                                                                               
         XR    R0,R0               YES, CLEAR OVERRIDE ELEM                     
VR120    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    VR125                                                            
         CLI   0(R6),DMFXELQ       DELETE X'10' OVERRIDE ELEMS                  
         BNE   VR120                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)                                  
*                                                                               
VR125    TM    MISCFLG1,MF1OVRDE   DID WE SELECT OVERRIDE?                      
         BZ    VR150                                                            
         TM    DOVFXNOH+4,X'08'    IS THIS INPUT FIELD NUMERIC?                 
         BNZ   VR127               YES                                          
         LA    R2,DOVFXNOH         NO, CAN'T SELECT IF NON-NUMERIC              
         B     INVLFLD                                                          
*                                                                               
VR127    XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING DMFXELD,R1                                                       
         MVI   DMFXEL,DMFXELQ      X'10' ELEM                                   
         MVI   DMFXLEN,DMFXLENQ                                                 
         MVC   DMFXOVRD,DOVFXNO    ONE TIME FAX OVERRIDE                        
         MVC   DMFXDATE,JDTTODAY   SAVE ACTIVE DATE                             
         MVC   SAVEFAX,DOVFXNO                                                  
         DROP  R1                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6) AND ADD                          
         B     VR150                                                            
*                                                                               
VR130    CLI   SAVEMTHD,C'E'       EMAIL?                                       
         BNE   VR140                                                            
         USING LIND,R2                                                          
         MVC   DMTHBDEC,LINROUT    SAVE THE COMMON NAME                         
         BAS   RE,SPACFILL         CAN'T OC BECAUSE OF CAPS                     
         MVC   SAVEBDEC,DMTHBDEC                                                
         B     VR150                                                            
         DROP  R2                                                               
*                                                                               
VR140    CLI   SAVEMTHD,C'I'       INBOX?                                       
         BE    *+6                                                              
         DC    H'0'                DIE, SHOULD BE I/F/E!!                       
         XC    DMTHBDEC,DMTHBDEC   DON'T NEED BDE, CLEAR IT                     
*                                                                               
VR150    BRAS  RE,UPDATTIM                                                      
*                                                                               
VR160    OC    SAVEKEY,SAVEKEY     DID WE FIND A LAST METHOD BEFORE?            
         BZ    VR170                                                            
*                                                                               
         L     R6,AIO                                                           
         USING DMTHKEY,R6                                                       
         OC    DMTHCLT,DMTHCLT     ALL CLIENT LAST METHOD RECORD?               
         BZ    VR165               YES                                          
         MVI   ACTELOPT,C'Y'                                                    
         GOTO1 PUTREC                                                           
         B     VR180                                                            
*                                                                               
VR165    MVC   DMTHCLT,BCLT        GOING TO ADD THE CLIENT SPECIFIC             
VR170    MVI   ACTELOPT,C'Y'                                                    
         GOTO1 ADDREC                                                           
*                                                                               
VR180    MVI   ACTELOPT,C'N'                                                    
         XC    8(3,R2),8(R2)                                                    
         MVI   8(R2),C'*'                                                       
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
         TM    MISCFLG1,MF1INVIT   EMAIL INVITE?                                
         BZ    VR190               NO                                           
         MVI   PFKEY,14            LET'S GO TO EMAIL/INVITE SCREEN              
         GOTO1 INITIAL,DMCB,SPFTABLE                                            
         DROP  R6                                                               
*                                                                               
VR190    OI    MISCFLG1,MF1RCCHG                                                
*                                                                               
VRX      DS    0H                                                               
         TM    MISCFLG1,MF1RCCHG   DID WE SELECT SOMETHING?                     
         BZ    VRXXIT                                                           
* UPDATE THE DOSPELEM IF NOT PREVIOUSLY CONFIRMED!!!                            
*                                                                               
         LA    R2,DOVORDH                                                       
*                                                                               
         BAS   RE,VALORDR                                                       
         BNE   VRXXIT              EXIT, INVALID ORDER NUMBER                   
*                                                                               
         LA    R2,KEY                                                           
         USING DOKEY,R2                                                         
         XC    KEY,KEY                                                          
         MVI   DOKTYPE,DOKTYPQ     X'0D'                                        
         MVI   DOKSUBTY,DOKSTYPQ   X'34'                                        
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORD                                                  
         DROP  R2                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE  DID I GET MY KEY?                     
         BNE   VRXXIT                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
* DELETE THE DOWIG AND DOEML                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,DOWIGELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VRX20                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6)  DELETE DOWIG!!                       
VRX20    L     R6,AIO                                                           
         MVI   ELCODE,DOEMLELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VRX30                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6)  DELETE DOEML!!                       
*                                                                               
VRX30    L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         USING DOSPELD,R6                                                       
         MVC   DOSPDEST,SAVEDEST                                                
         MVC   DOSPMTHD,SAVEMTHD                                                
         DROP  R6                                                               
*                                                                               
         CLI   SAVEMTHD,C'I'       AM I GOING INBOX NOW?                        
         BE    VRX90               YES, PUTREC!!                                
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         ZICM  R1,DORLEN,2                                                      
         AR    R6,R1               POINT TO END OF RECORD                       
*                                                                               
         CLI   SAVEMTHD,C'E'       EMAIL?                                       
         BNE   VRX50                                                            
         LA    R4,DOEMELEM         YES, ADD DOEMELEM                            
         USING DOEMLD,R4                                                        
         MVI   DOEMLEL,DOEMLELQ                                                 
         MVI   DOEMLLEN,DOEMLLNQ                                                
         MVC   DOEMLBDE,SAVEBDEC                                                
         GOTO1 RECUP,DMCB,(C'S',AIO),DOEMELEM,(R6)                              
         B     VRX55                                                            
*                                                                               
VRX50    CLI   SAVEMTHD,C'F'       FAX?                                         
         BNE   VRX90               NO??? JUST PUTREC                            
         TM    MISCFLG1,MF1OVRDE   IF NOT FAX OVERRRIDE?                        
         BZ    VRX90               THEN DON'T ADD DOWIG                         
*                                                                               
VRX55    LA    R4,DOWGELEM                                                      
         USING DOWIGELD,R4                                                      
         MVI   DOWIGEL,DOWIGELQ                                                 
         MVI   DOWIGLEN,DOWIGLNQ                                                
         MVC   DOWIGMTH,SAVEMTHD                                                
         XC    DOWIGEML,DOWIGEML                                                
         MVC   DOWIGFXN,SAVEFAX                                                 
         DROP  R4                                                               
*                                                                               
VRX60    GOTO1 RECUP,DMCB,(C'S',AIO),DOWGELEM,(R6)                              
*                                                                               
VRX90    GOTO1 PUTREC                                                           
*                                                                               
VRXXIT   B     EXIT                                                             
         EJECT                                                                  
***************                                                                 
* FILL ALL CHARACTERS LESS THEN OR EQUAL TO SPACE WITH SPACE                    
*                                                                               
*  ON ENTRY: R6    A(DMTHELQ ELEM)                                              
***************                                                                 
SPACFILL NTR1                                                                   
         USING DMTHELD,R6                                                       
         LA    RE,DMTHBDEC+L'DMTHBDEC-1                                         
         LA    R1,L'DMTHBDEC                                                    
SFILL10  CLI   0(RE),C' '                                                       
         BH    EXIT                                                             
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                                                             
         BCT   R1,SFILL10                                                       
         B     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
* READ FOR DELETED/UPDATE THE LAST METHOD KEY STORED IN SVKEY IN CASE           
* WE NEED TO UNDELETED THE KEY AND REC                                          
***********************************************************************         
RDDSVKEY NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         TM    KEY+13,X'80'        DELETED?                                     
         BZ    RDSVK10                                                          
         NI    KEY+13,X'FF'-X'80'  NOT DELETED ANYMORE                          
         GOTO1 WRITE                                                            
*                                                                               
RDSVK10  MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 GETREC              GET LAST METHOD RECORD                       
         MVI   RDUPDATE,C'N'                                                    
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
         L     R6,AIO                                                           
         USING DMTHKEY,R6                                                       
         NI    15(R6),X'FF'-X'80'                                               
*                                                                               
RDSVKX   B     EXIT                                                             
*============================================================                   
* CONVERTS EBCDIC ORDER NUMBER TO BINARY ORDER NUMBER                           
*                                                                               
* ON ENTRY: R2 POINTS TO ORDER NUMBER FLDHDR                                    
*                                                                               
* ON EXIT:  BINARY ORDER NUMBER IN BINORD                                       
*           CC OF NOT EQUAL IF INVALID ORDER NUMBER                             
*============================================================                   
         SPACE 1                                                                
VALORDR  NTR1                                                                   
         XC    BINORD,BINORD                                                    
         CLI   DOVORDH+5,8         INPUT LENGTH OF 8?                           
         BNE   VALORDNO            NO, INVALID!!                                
*&&DO                                                                           
         TM    DOVORDH+4,X'08'     VALID NUMERIC?                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,8(R2),DUB,8   GET ORDER AS HEX                        
*                                                                               
         MVC   PACKOF4B,DUB        CONVERT IT TO PACKED                         
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2   COPY CURRENT CENTURY                      
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),DUB   STICK IN DAYS IN YEAR                        
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,8+4(4,R2)       SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
         B     YES                                                              
*                                                                               
VALORDNO B     NO                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*  ON ENTRY: AIO CONTAINS X'0D34' ORDER RECORD                                  
*                                                                               
*  ON EXIT:  CC RETURNED - WAS ORDER PREVIOUSLY CONFIRMED??                     
*                                                                               
***********************************************************************         
PVCNFM   NTR1                                                                   
*  CHECK THE REVISION NUMBER FIRST BEFORE LOOPING EL 11'S                       
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ        X'03' - SUPPLEMENTARY ID ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   PVCNF10                                                          
         USING DOSPELD,R6                                                       
         CLI   DOSPIMTH,C'E'       NO, PREV SEND WAS WITH EMAIL?                
         BE    PVCNFNO                 YES, DON'T CONSIDER AS CONFIRMED         
         DROP  R6                                                               
*                                                                               
PVCNF10  L     R6,AIO                                                           
         MVI   ELCODE,DOSTELQ        X'12' - TRANSMISSION ELEMENT               
         BAS   RE,GETEL              IS THERE IF WE TRANSMITTED ORDER           
         B     *+8                                                              
PVCNF20  BAS   RE,NEXTEL                                                        
         BNE   PVCNFNO                                                          
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,QEMPTY       C'E', EMPTY ORDER                          
         BE    PVCNFNO                                                          
         OI    MISCFLG2,MF2PRSNT   ITS PREVIOUSLY SENT                          
*                                                                               
PVCNF25  CLI   DOSTSTAT,QCFMD        C'C', CONFIRMED                            
         BE    PVCNF30                IT'S CONFIRMED                            
         CLI   DOSTSTAT,QBYRCNFM     C'H', ORDER FAXED AND CONFIRMED            
         BE    PVCNF30                IT'S CONFIRMED                            
         CLI   DOSTSTAT,QNODARE      C'N', NOTDARED                             
         BE    PVCNF30                                                          
         CLI   DOSTSTAT,QUNDARE    C'U', UNDARED?                               
         BNE   PVCNF20                                                          
PVCNF30  OI    MISCFLG2,MF2NOINB   ITS PREVIOUSLY CONFIRMED                     
*                                                                               
PVCNFYES B     YES                                                              
*                                                                               
PVCNFNO  B     NO                                                               
         EJECT                                                                  
RELO     DS    A                                                                
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                                       
***********************************************************************         
SPFTABLE  DS    0C                                                              
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
* EMAIL/INVITE                                                                  
         DC    AL1(SPF14X-*,14,0,0,(SPF14X-SPF14)/KEYLNQ,0)                     
         DC    CL3' ',CL8'EMAIL',CL8'INVITE'                                    
SPF14    DC    AL1(KEYTYTWA,L'DOVMED-1),AL2(DOVMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'DOVBYR-1),AL2(DOVBYR-T234FFD)                     
         DC    AL1(KEYTYTWA,L'DOVSTA-1),AL2(DOVSTA-T234FFD)                     
         DC    AL1(KEYTYTWA,L'DOVCLT-1),AL2(DOVCLT-T234FFD)                     
SPF14X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
OOPTION  MVI   GERROR1,ONEOPTN     ONE OPTION AT A TIME PLEASE                  
         B     ERREXIT                                                          
*                                                                               
INVLREFN MVI   GERROR1,INVREFN                                                  
         B     ERREXIT                                                          
*                                                                               
NOTXTERR MVI   GERROR1,TXTREQ      AT LEAST ONE LINE OF TEXT REQUIRED           
         B     ERREXIT                                                          
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
***********************************************************************         
* BUILD LAST METHOD RECORD BASED ON THE CONFIRMED ORDER INFO                    
*                                                                               
* ON ENTRY:    AIO                 A(ORDER RECORD)                              
*                                                                               
* ON EXIT:     AIO3                A(FAKE LAST METHOD RECORD)                   
*                                                                               
* *** WARNING ***                                                               
*   ELEM GETS CLOBBERED                                                         
***********************************************************************         
FAKELMRC NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO3                                                          
         LHI   RF,LIOS                                                          
         XCEFL                                                                  
*                                                                               
         L     R2,AIO3                                                          
         USING DMTHKEY,R2                                                       
         MVI   DMTHTYPE,DMTHTYPQ   X'0D3E'                                      
         MVI   DMTHSBTY,DMTHSBTQ                                                
         MVC   DMTHAGMD,BAGYMD                                                  
         MVI   DMTHLEN+1,DMTHFRST-DMTHKEY                                       
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         MVC   DMTHBYR,DOIDBYR                                                  
         MVC   DMTHSTA,DOISTA                                                   
         MVC   DMTHCLT,DOIDCLT                                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING DMTHELD,R3                                                       
         MVI   DMTHEL,DMTHELQ                                                   
         MVI   DMTHELLN,DMTHLNQ                                                 
*                                                                               
         L     R6,AIO              SEE IF ORDER HAS DEST/METH                   
         MVI   ELCODE,DOSPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   FLMR10              NONE                                         
         USING DOSPELD,R6                                                       
         CLI   DOSPDEST,0                                                       
         BE    FLMR10                                                           
         CLI   DOSPIMTH,C'E'       ORIGINALLY SENT EMAIL?                       
         BNE   FLMR05                                                           
         MVC   DMTHDEST,DOSPDEST   YES, USER MIGHT HAVE CHANGED SINCE           
         MVC   DMTHMTHD,DOSPMTHD       OVERRIDE IF EMAIL/CONFIRMED              
         B     FLMR10                                                           
*                                                                               
FLMR05   MVC   DMTHDEST,DOSPDEST   USE DEST/MTHD OF SEND BEFORE CONFIRM         
         MVC   DMTHMTHD,DOSPMTHD                                                
*                                                                               
FLMR10   LA    R4,DMTHFRST         ADD LAST METHOD ELEM                         
         GOTO1 RECUP,DMCB,(C'S',AIO3),ELEM,(R4)                                 
         LR    R3,R4               R3 = A(LAST METHOD ELEM IN AIO3)             
*                                                                               
         CLI   DMTHMTHD,C'I'       WENT TO AN INBOX?                            
         BE    FLMRX               SHOULDN'T BE A PROBLEM                       
*                                                                               
         L     R6,AIO              LET'S SEE IF WE CAN FIND OUT WHERE           
         MVI   ELCODE,DOWIGELQ      THE ORDER WENT                              
         BRAS  RE,GETEL                                                         
         BNE   FLMRX                                                            
         USING DOWIGELD,R6                                                      
         CLI   DOWIGMTH,C'E'                                                    
         BNE   FLMR20                                                           
         L     R6,AIO              LET'S SEE IF WE CAN FIND OUT WHERE           
         MVI   ELCODE,DOEMLELQ      THE ORDER WENT                              
         BRAS  RE,GETEL                                                         
         BNE   FLMRX                                                            
         USING DOEMLD,R6                                                        
         MVC   DMTHBDEC,DOEMLBDE                                                
         LA    R1,DMTHBDEC+L'DMTHBDEC-1                                         
FLMR15   CLI   0(R1),C' '                                                       
         BH    FLMRX                                                            
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         B     FLMR15                                                           
         DROP  R6                                                               
*                                                                               
         USING DOWIGELD,R6                                                      
FLMR20   CLI   DOWIGMTH,C'F'                                                    
         BNE   FLMRX                                                            
         OI    DMTHFLG1,DMF1KFAX   KEEP FAX METHOD, SKIP BDE                    
*****                                                                           
         CLI   DMTHDEST,0          NOTHING HERE? (DOSPDEST=0)                   
         BNE   FLMR30              WE GOT SOMETHING                             
         MVI   DMTHDEST,C'S'       COULDN'T FAX BACK THEN TO REP BEFORE         
         MVI   DMTHMTHD,C'F'           KEEPING DOSPDEST (DESTOV)                
*&&DO                                                                           
FLMR30   CLC   DOWIGFXA(L'DOWIGFXA+L'DOWIGFXT),SPACES                           
         BH    FLMRX               NEED ATTENTION AND PHONE FOR LATER           
*&&                                                                             
FLMR30   XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING DMFXELD,R3                                                       
         MVI   DMFXEL,DMFXELQ                                                   
         MVI   DMFXLEN,DMFXLENQ                                                 
         MVC   DMFXOVRD,DOWIGFXN   TREAT AS A FAX OVERRIDE #                    
         MVC   DMFXDATE,JDTTODAY   SAVE ACTIVE DATE                             
         DROP  R3                                                               
         XR    R0,R0                                                            
         IC    R0,1(R4)            WHERE TO INSERT TEMP FAX OVRDE ELEM          
         AR    R4,R0                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO3),ELEM,(R4)                                 
*                                                                               
FLMRX    J     EXIT                                                             
         DROP  R2,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE THE NEW TIME STAMP ELEMENT                                             
***********************************************************************         
UPDATTIM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING DATTIMD,R2                                                       
         MVI   DATTIM,DATTIMLQ     X'D1'                                        
         MVI   DATTIMLN,DATTMLNQ                                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,DATTMGDT)                                  
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    UPDT10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
         GOTO1 DATCON,DMCB,(0,DUB),(19,DATTMGDT)                                
*                                                                               
UPDT10   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DATTMGTM                                                    
         MVC   DATTMCDT,DATTMGDT                                                
         MVC   DATTMCTM,DATTMGTM                                                
*                                                                               
         L     R6,AIO              WE DON'T KNOW CREATION DATE SO WE'LL         
         MVI   ELCODE,X'F1'           USE THE X'F1' ELEM TO GET IT              
         BRAS  RE,GETEL                                                         
         BNE   UPDT15                                                           
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(19,DATTMCDT)                           
         DROP  R6                                                               
*                                                                               
UPDT15   L     R6,AIO                                                           
         MVI   ELCODE,DATTIMLQ     X'D1'                                        
         BRAS  RE,GETEL            DO WE HAVE ONE?                              
         BNE   UPDT20              NO, ADD IT                                   
OLD      USING DATTIMD,R6                                                       
         MVC   DATTMCDT,OLD.DATTMCDT                                            
         MVC   DATTMCTM,OLD.DATTMCTM                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)    DELETE OLD DATTIMELEM         
UPDT20   GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)    ADD NEW DATTIMELEM            
         DROP  OLD,R2                                                           
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPOMSDSCTS                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENDESTN                                                     
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSC7D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRMTH                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFAX                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE GEGENBDE                                                       
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
SAVEKEY  DS    CL13                                                             
SAVEDEST DS    CL1                 SAVED DESTINATION                            
SAVEMTHD DS    CL1                 SAVED METHOD FOR THE DESTINATION             
SAVEBDEC DS    CL40                SAVE BDE COMMON NAME                         
SAVEFAX  DS    CL16                SAVE FAX NUMBER                              
SVFXPROF DS    CL16                 FX PROFILE                                  
*                                                                               
BINORD   DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                     DATE PORTION                             
BINORDSQ DS    XL2                     SEQUENCE PORTION                         
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
*                                                                               
MISCFLG1 DS    CL1                  MISC FLAGS                                  
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1RCCHG EQU   X'40'                - RECORD CHANGED                            
MF1DOV   EQU   X'20'                - 1+ OPTIONS SELECTED IN DESTOV             
MF1INBOX EQU   X'10'                - DEFAULT IS INDEED INBOX                   
MF1NOINB EQU   X'08'                - NO INBOX ALLOWABLE                        
MF1OVRDE EQU   X'04'                - FAX OVERRIDE                              
MF1INVIT EQU   X'02'                - EMAIL INVITE                              
*                                                                               
MISCFLG2 DS    CL1                 MISC FLAGS 2                                 
MF2RADIO EQU   X'80'                - WE HAVE A RADIO ORDER                     
MF2FXRAD EQU   X'40'                - FAX ALL RADIO ORDERS PROFILE SET          
MF2NOINB EQU   X'20'                - NO INBOX, ORDER PREV CNFM/NTDARE          
MF2PRSNT EQU   X'10'                - PREVIOUSLY SENT ORDER                     
*                                                                               
DOWGELEM DS    XL(DOWIGELQ)                                                     
DOEMELEM DS    CL(DOEMLLNQ)                                                     
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMFXRAD EQU   PROFOM+3             - FAX RADIO ALL RADIO ORDERS?               
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
LIND     DSECT                                                                  
LINSELH  DS    CL8                                                              
LINSEL   DS    CL3                                                              
LINROUTH DS    CL8                                                              
LINROUT  DS    CL41                                                             
         DS    CL30                USED TO BE COMPANY                           
LINNEXTL DS    0H                                                               
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SPOMS17   01/16/07'                                      
         END                                                                    
