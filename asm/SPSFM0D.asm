*          DATA SET SPSFM0D    AT LEVEL 040 AS OF 05/01/02                      
*PHASE T2170DA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2170D - SIR STATION DEFAULT UPGRADE MAINTENANCE      *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST, *         
*               AND PRINT REPORT                                      *         
*  INPUTS       SCREEN T217BD (MAINTENANCE)                           *         
*               SCREEN T217AD (LIST)                                  *         
*               SCREEN T217CD (REPORT)                                *         
*                                                                     *         
*  OUTPUTS      UPDATED DEFAULT UPGRADE RECORDS                       *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - DEFAULT UPGRADE RECORD                          *         
*               IO2 - MISC.                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2170D - SIR STATION DEFAULT UPGRADE RECORDS'                   
T2170D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2170D                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         USING T2170D,RB,R7                                                     
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    MAIN10                                                           
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    MAIN10                                                           
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN10                                                           
*                                                                               
NOTAUTHD MVI   GETMSYS,2           CHANGE TO X'02' ERROR SYSTEM                 
         LA    R2,CONACTH          ACTION                                       
         B     TRAPERR                                                          
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R5,SVKEY                                                         
         USING SIRKEY,R5                                                        
         XC    SVKEY,SVKEY                                                      
         MVI   SIRKTYPE,SIRKTYPQ   SIR RECORD TYPE                              
*                                                                               
         LA    R2,=X'0900000000010000E3'                                        
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
         MVC   SIRKAM,BAGYMD                                                    
*                                                                               
         LA    R2,SIRSCHH          SCHEME                                       
         CLI   ACTNUM,ACTREP       TEST ACTION REPORT                           
         BNE   *+8                 NO                                           
         LA    R2,SIRSTAH          YES -- FIRST FIELD IS REALLY SCHEME          
*                                                                               
         XC    SCHEME,SCHEME                                                    
         CLI   5(R2),0             TEST SCHEME GIVEN                            
         BNE   VK10                YES                                          
*                                                                               
         XC    WORK,WORK           READ SID PROFILE FOR AGENCY/MEDIA            
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    VK5                 NO -- NO SCHEME IS OK                        
         CLI   WORK+2,C'N'         DEFAULT ALLOWED?                             
         BNE   VK5                 YES                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK5      CLI   ACTNUM,ACTREP       TEST ACTION REPORT                           
         BE    VKX                 YES - PRINT ALL SCHEMES                      
         MVC   8(3,R2),=C'ALL'     NO - LIST OR DISPLAY SCHEME 'ALL'            
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VK20                                                             
*                                                                               
VK10     CLC   =C'ALL',8(R2)       TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK20                                                             
         OC    8(3,R2),=C'   '                                                  
         GOTO1 CLPACK,DMCB,8(R2),SIRKCODE                                       
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BE    VK20                YES                                          
         MVI   ERROR,INVSCH                                                     
         B     TRAPERR                                                          
*                                                                               
VK20     MVC   SCHEME,SIRKCODE     SAVE THE SCHEME CODE                         
         CLI   ACTNUM,ACTREP       TEST REPORT                                  
         BE    VKX                 YES -- NOTHING ELSE TO VALIDATE              
*                                                                               
         MVI   REPMUSER,C'N'       ASSUME USER DOESN'T USE REP MARKETS          
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         CLC   =C'ALL',8(R2)                                                    
         BE    *+10                                                             
         MVC   WORK+23(3),8(R2)                                                 
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+10                NO                                           
         MVC   REPMUSER,WORK+1                                                  
*                                                                               
         XC    BMKTSTA,BMKTSTA     MARKET/STATION                               
         XC    SIRDMKT,SIRDMKT                                                  
         LA    R2,SIRSTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK60                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK30     TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    VK40                                                             
*                                                                               
         CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   VK32                NO                                           
         GOTO1 VALIRMKT            YES                                          
         B     VK35                                                             
VK32     GOTO1 VALIMKT             IT'S A MARKET NUMBER                         
*                                                                               
VK35     PACK  DUB,QMKT                                                         
         CVB   R4,DUB                                                           
         STCM  R4,3,SIRKMKT        SAVE THE MARKET                              
         B     VK50                                                             
*                                                                               
VK40     CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   VK42                NO                                           
         GOTO1 VALIRSTA            YES                                          
         B     VK45                                                             
VK42     GOTO1 VALISTA                                                          
VK45     MVC   SIRKMS,BMKTSTA                                                   
*                                                                               
VK50     MVC   SIRDMKT,MKTNM       DISPLAY MARKET NAME                          
VK60     OI    SIRDMKTH+6,X'80'    XMIT                                         
*                                                                               
         LA    R2,SIRSCHH          SCHEME                                       
         LA    R5,KEY              CREATE THE SCHEME KEY                        
         MVC   KEY,SVKEY                                                        
         XC    SIRKMS,SIRKMS                                                    
         MVI   SIRKMON,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST SCHEME EXISTS                           
         BE    *+12                YES                                          
         MVI   ERROR,NOSCHM                                                     
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING EDYELEM,R6                                                       
         MVI   ELCODE,EDYCODEQ     DEFAULT YEAR ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURYEAR,EDYYEAR     GET THE YEAR                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         STH   R1,NUMPERS                                                       
         LA    R6,2(R6)            FIRST PERIOD NAME                            
*                                                                               
         XC    SVPERIOD,SVPERIOD   TABLE OF SAVED PERIODS                       
         LA    R3,SVPERIOD                                                      
VK70     MVC   0(4,R3),1(R6)       SAVE EACH NAME IN TABLE                      
         LA    R3,4(R3)                                                         
         LA    R6,5(R6)                                                         
         BCT   R1,VK70                                                          
*                                                                               
         MVI   PERNUM,0            PERIOD                                       
         LA    R2,SIRPERH                                                       
         CLI   5(R2),0                                                          
         BNE   VK80                                                             
*                                                                               
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VKX                                                              
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK80     LA    R3,SVPERIOD                                                      
         OC    SIRPER,=C'    '     PAD NAME WITH BLANKS                         
         LH    R0,NUMPERS                                                       
         LA    R1,1                                                             
*                                                                               
VK90     CLC   SIRPER,0(R3)        LOOK FOR THE PERIOD NAME                     
         BE    VK100                                                            
         LA    R3,4(R3)            NEXT ENTRY                                   
         LA    R1,1(R1)                                                         
         BCT   R0,VK90                                                          
         LA    R2,SIRPERH                                                       
         MVI   ERROR,INVBUYP                                                    
         B     TRAPERR                                                          
*                                                                               
VK100    STC   R1,PERNUM           SAVE THE PERIOD NUMBER                       
*                                                                               
VKX      MVC   KEY,SVKEY           CREATE THE UPGRADE KEY                       
         MVC   SIRKMON,PERNUM                                                   
         OI    SIRKMON,SIRKBUYQ                                                 
*                                                                               
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,EUPCODEQ     UPGRADE                                      
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SIRUPGH          UPGRADE FIELD                                
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R6,MYWORK           BUILD ELEMENT IN MYWORK                      
         USING EUPELEM,R6                                                       
         XC    MYWORK,MYWORK                                                    
         MVI   EUPCODE,EUPCODEQ                                                 
         MVI   EUPLEN,EUPLENEQ                                                  
*                                                                               
         GOTO1 VALIUPG,DMCB,BLOCK+256                                           
         LA    R5,BLOCK+256                                                     
         USING SPDEMUPD,R5                                                      
         MVC   EUPUPFIL,SPUPFIL                                                 
         MVC   EUPUPGRD,SPUPTYPE                                                
         MVC   EUPUPFBK,SPUPFBK                                                 
         MVC   EUPUPINP,SPUPPRG                                                 
         DROP  R5                                                               
*                                                                               
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         CLI   EUPUPGRD,SPUPTPUT   TEST HUT OR PUT                              
         BNE   VR10                NO, NEITHER ONE                              
         CLI   EUPUPGRD+1,C'P'     TEST PUT                                     
         BNE   VR10                NO, IT'S A HUT                               
*                                                                               
         XC    EUPUPINP,EUPUPINP   EBCDIC UPGRADE IS INVALID                    
         ZIC   R1,EUPUPGRD+2       PUT BOOK YEAR                                
         SR    R1,R0               DETERMINE RELATIVE YEAR                      
         STC   R1,EUPUPGRD+2       SAVED IN 1-BYTE 2'S COMPLEMENT FORM          
*                                                                               
VR10     OC    EUPUPFBK,EUPUPFBK   TEST FOR SHARE BOOK                          
         BZ    VR20                                                             
         ZIC   R1,EUPUPFBK         SHARE BOOK YEAR                              
         SR    R1,R0               DETERMINE RELATIVE YEAR                      
         STC   R1,EUPUPFBK         SAVED IN 1-BYTE 2'S COMPLEMENT FORM          
         DROP  R6                                                               
*                                                                               
VR20     XC    ELEM,ELEM                                                        
         MVC   ELEM(EUPLENEQ),MYWORK                                            
         GOTO1 ADDELEM                                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,EUPCODEQ     UPGRADE EXPRESSION                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SIRUPGH          UPGRADE FIELD                                
         USING EUPELEM,R6                                                       
*                                                                               
         MVI   WORK,C' '           BUILD UPGRADE EXPRESSION                     
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EUPUPFIL                                               
*                                                                               
         CLI   EUPUPGRD,SPUPTPUT   TEST HUT OR PUT                              
         BNE   DR10                NO, NEITHER ONE                              
         CLI   EUPUPGRD+1,C'P'     TEST PUT                                     
         BNE   DR10                NO, IT'S A HUT                               
*                                                                               
         MVC   WORK+4(4),=C'PUT/'                                               
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPGRD+2     RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPGRD+2                                                    
         GOTO1 DATCON,DMCB,(3,EUPUPGRD+2),(6,WORK+8)                            
         MVC   WORK+11(2),WORK+12  GET RID OF '/'                               
         MVI   WORK+13,C' '                                                     
         B     *+10                                                             
*                                                                               
DR10     MVC   WORK+4(16),EUPUPINP                                              
*                                                                               
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    DR20                                                             
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPFBK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPFBK                                                      
*                                                                               
         LA    R5,WORK+21                                                       
         CLI   0(R5),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','          EDIT IN COMMA                                
         LA    R5,2(R5)                                                         
         MVC   0(3,R5),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R5))                               
*                                                                               
DR20     ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        EBCDIC UPGRADE EXPRESSION                    
         OI    6(R2),X'80'         XMIT                                         
         DROP  R6                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK2                                                              
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK2                                                              
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTHD                                                         
         CLI   THISLSEL,C'D'       SELECT FOR DELETE?                           
         BE    NOTAUTHD                                                         
*                                                                               
DK2      L     R6,AIO              RECORD SELECTED                              
         USING SIRREC,R6                                                        
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(L'KEY),KEY                                                
*                                                                               
         OI    SIRSTAH+6,X'80'     XMIT                                         
         XC    SIRSTA,SIRSTA                                                    
         GOTO1 MSUNPK,DMCB,(X'80',SIRKMS),WORK,WORK+4                           
         OC    SIRKSTA,SIRKSTA     TEST STATION 'ALL'                           
         BNZ   *+14                                                             
         MVC   SIRSTA(4),WORK      MARKET NUMBER                                
         B     DK5                                                              
         MVC   SIRSTA,WORK+4       STATION NAME                                 
         CLI   SIRSTA,X'F0'        CABLE HEADEND?                               
         BL    DK5                                                              
         MVI   SIRSTA+4,C'/'       YES                                          
*                                                                               
DK5      CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   DK20                NO                                           
*                                                                               
         GOTO1 SWITCH,DMCB,=C'REP',0                                            
         CLI   DMCB+4,2            TEST REP SYSTEM IS STARTED                   
         BNE   *+12                YES                                          
         MVI   ERROR,REPISOFF                                                   
         B     TRAPERR                                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         LA    R5,KEY              READ MARKET KEY                              
         USING RMKTRECD,R5                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,WORK                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0             TEST VALID MARKET                            
         BE    *+14                YES                                          
         MVC   SIRDMKT,=CL24'*** UNKNOWN ***'                                   
         B     DK10                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'REPFIL',KEY+28,          +        
               AIO2,DMWORK                                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO2             DISPLAY MARKET NAME                          
         MVC   SIRDMKT(L'RMKTNAME),RMKTNAME                                     
         DROP  R5                                                               
*                                                                               
DK10     GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    DK40                YES                                          
         DC    H'0'                                                             
*                                                                               
DK20     MVI   KEY,C' '            BUILD MARKET RECORD KEY                      
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVI   KEY+1,C'T'          MEDIA 'T'                                    
         MVC   KEY+2(4),WORK                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO2                                                          
         USING MKTRECD,R5                                                       
         CLC   KEY(8),0(R5)        DO WE NEED TO READ THE RECORD                
         BE    DK30                NO, WE HAVE ALREADY READ IT                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO2                 
DK30     LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   SIRDMKT,0(R1)                                                    
         DROP  R5                                                               
*                                                                               
DK40     OI    SIRDMKTH+6,X'80'    XMIT MARKET NAME                             
         XC    KEY,KEY             RESTORE KEY                                  
         MVC   KEY,MYWORK                                                       
         OI    SIRPERH+6,X'80'     XMIT PERIOD NAME                             
         XC    SIRPER,SIRPER                                                    
         LA    R3,SVPERIOD         BEGINNING OF PERIOD TABLE                    
         SH    R3,=H'4'            BACK UP TO BEFORE TABLE START                
         MVC   BYTE,SIRKMON                                                     
         NI    BYTE,X'FF'-SIRKBUYQ                                              
         ZIC   R0,BYTE                                                          
*                                                                               
         LA    R3,4(R3)            FIND DATES IN TABLE                          
         BCT   R0,*-4                                                           
         MVC   SIRPER,0(R3)        DISPLAY PERIOD NAME                          
*                                                                               
         OI    SIRSCHH+6,X'80'     XMIT                                         
         OC    SIRKCODE,SIRKCODE   TEST SCHEME 'ALL'                            
         BNZ   *+14                                                             
         MVC   SIRSCH,=C'ALL'                                                   
         B     DKX                                                              
         GOTO1 CLUNPK,DMCB,SIRKCODE,SIRSCH                                      
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING SIRKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   SIRKTYPE,SIRKTYPQ   RECORD TYPE                                  
         MVC   SIRKAM,BAGYMD       AGY/MED                                      
         MVC   SIRKCODE,SCHEME     SCHEME                                       
         MVC   SIRKMS,BMKTSTA      MKT/STA                                      
         MVC   SIRKMON,PERNUM      PERIOD                                       
         OI    SIRKMON,SIRKBUYQ                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         MVI   RDUPDATE,C'N'                                                    
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(4),SAVEKEY      TEST SAME TYPE/AGY/MED/SCHEME                
         BNE   LRX                                                              
         OC    KEY+9(3),KEY+9      TEST STATION RECORD                          
         BNZ   LR20                                                             
*                                                                               
         OC    BMKT,BMKT           TEST A MARKET WAS GIVEN                      
         BZ    *+14                                                             
         CLC   SIRKMKT,BMKT        TEST SAME MARKET                             
         BNE   LR20                NO - NEXT RECORD                             
*                                                                               
         OC    BSTA,BSTA           TEST A STATION WAS GIVEN                     
         BZ    *+14                                                             
         CLC   SIRKSTA,BSTA        TEST SAME STATION                            
         BNE   LR20                NO - NEXT RECORD                             
*                                                                               
         CLI   PERNUM,0            TEST PERIOD WAS GIVEN                        
         BE    LR40                                                             
         MVC   BYTE,PERNUM                                                      
         OI    BYTE,SIRKBUYQ                                                    
         CLC   SIRKMON,BYTE        TEST SAME PERIOD                             
         BNE   LR20                                                             
*                                                                               
LR40     XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',SIRKMS),LSTMKT,WORK                           
         MVC   SVKEY(13),KEY       SAVE STATION UPGRADE KEY                     
*                                                                               
         CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   LR60                NO                                           
*                                                                               
         GOTO1 SWITCH,DMCB,=C'REP',0                                            
         CLI   DMCB+4,2            TEST REP SYSTEM IS STARTED                   
         BNE   *+12                YES                                          
         MVI   ERROR,REPISOFF                                                   
         B     TRAPERR                                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         LA    R5,KEY              READ MARKET KEY                              
         USING RMKTRECD,R5                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,LSTMKT                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0             TEST VALID MARKET                            
         BE    *+14                YES                                          
         MVC   LSTMKTNM,=CL24'*** UNKNOWN ***'                                  
         B     LR50                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'REPFIL',KEY+28,          +        
               AIO2,DMWORK                                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO2             DISPLAY MARKET NAME                          
         MVC   LSTMKTNM(L'RMKTNAME),RMKTNAME                                    
         DROP  R5                                                               
*                                                                               
LR50     GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    LR80                YES                                          
         DC    H'0'                                                             
*                                                                               
LR60     MVI   KEY,C' '            BUILD MARKET RECORD KEY                      
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVI   KEY+1,C'T'          MEDIA 'T'                                    
         MVC   KEY+2(4),LSTMKT                                                  
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO2                                                          
         USING MKTRECD,R5                                                       
         CLC   KEY(8),0(R5)        DO WE NEED TO READ THE RECORD                
         BE    LR70                NO, WE HAVE ALREADY READ IT                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO2                 
LR70     LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   LSTMKTNM,0(R1)                                                   
         DROP  R5                                                               
*                                                                               
LR80     XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       RESTORE STATION UPGRADE KEY                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RESTORE DMGR SEQUENCE                        
         OC    SIRKSTA,SIRKSTA     TEST ANY STATION                             
         BNZ   *+14                                                             
         MVC   LSTSTA,=CL8'ALL'                                                 
         B     LR85                                                             
         MVC   LSTSTA,WORK         STATION                                      
         CLI   LSTSTA,X'F0'        CABLE HEADEND?                               
         BL    LR85                                                             
         MVI   LSTSTA+4,C'/'       YES                                          
*                                                                               
LR85     LA    R3,SVPERIOD         BEGINNING OF PERIOD TABLE                    
         SH    R3,=H'4'            BACK UP TO BEFORE TABLE START                
         MVC   BYTE,SIRKMON                                                     
         NI    BYTE,X'FF'-SIRKBUYQ                                              
         ZIC   R0,BYTE                                                          
         LA    R3,4(R3)            FIND NAME IN TABLE                           
         BCT   R0,*-4                                                           
         MVC   LSTPER,0(R3)        PERIOD NAME                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING EUPELEM,R6                                                       
*                                                                               
         MVI   WORK,C' '           BUILD UPGRADE EXPRESSION                     
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EUPUPFIL                                               
*                                                                               
         CLI   EUPUPGRD,SPUPTPUT   TEST HUT OR PUT                              
         BNE   LR90                NO, NEITHER ONE                              
         CLI   EUPUPGRD+1,C'P'     TEST PUT                                     
         BNE   LR90                NO, IT'S A HUT                               
*                                                                               
         MVC   WORK+4(4),=C'PUT/'                                               
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPGRD+2     RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPGRD+2                                                    
         GOTO1 DATCON,DMCB,(3,EUPUPGRD+2),(6,WORK+8)                            
         MVC   WORK+11(2),WORK+12  GET RID OF '/'                               
         MVI   WORK+13,C' '                                                     
         B     *+10                                                             
*                                                                               
LR90     MVC   WORK+4(16),EUPUPINP                                              
*                                                                               
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    LR100                                                            
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPFBK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPFBK                                                      
*                                                                               
         LA    R5,WORK+21                                                       
         CLI   0(R5),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','          EDIT IN COMMA                                
         LA    R5,2(R5)                                                         
         MVC   0(3,R5),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R5))                               
*                                                                               
LR100    MVC   LSTUPG,WORK                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    KEY,KEY             CLEAR KEY STORAGE                            
         LA    R4,KEY                                                           
         USING SIRKEY,R4                                                        
*                                                                               
         MVI   SIRKTYPE,SIRKTYPQ   RECORD TYPE                                  
         MVC   SIRKAM,BAGYMD       A/M INFO                                     
         MVC   SIRKCODE,SCHEME     SCHEME                                       
         MVC   SAVEKEY,KEY                                                      
         XC    TMAR,TMAR                                                        
         XC    TSTA,TSTA                                                        
         MVC   SCHEME,=X'FFFF'     SO THERE'S NO MATCH THE FIRST TIME           
*                                                                               
         MVI   FIRSTIME,C'Y'       FIRST TIME THROUGH                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         B     PR20                                                             
*                                                                               
PR10     LA    R4,KEY              NEXT RECORD                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR20     CLC   KEY(2),SAVEKEY      TEST SAME TYPE/A/M                           
         BNE   PR180                                                            
         CLC   KEY+2(2),SAVEKEY+2  TEST SAME SCHEME                             
         BE    *+12                                                             
         CLI   SIRSTAH+5,0         TEST SCHEME FILTER GIVEN                     
         BNE   PR180               YES (STATION FIELD IS REALLY SCHEME)         
*                                                                               
         OC    SIRKMS,SIRKMS       MUST HAVE MKT/STA                            
         BZ    PR10                                                             
         OC    KEY+9(3),KEY+9      TEST STATION RECORD                          
         BNZ   PR10                                                             
*                                                                               
         MVC   SVKEY(13),KEY       SAVE STATION UPGRADE KEY                     
         LA    R4,KEY              BUILD SCHEME KEY                             
         XC    SIRKMS,SIRKMS                                                    
         MVI   SIRKMON,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST SCHEME KEY FOUND                        
         BNE   PR10                NO                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING EDYELEM,R6                                                       
         MVI   ELCODE,EDYCODEQ     DEFAULT YEAR ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURYEAR,EDYYEAR     SAVE CURRENT YEAR                            
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         STH   R1,NUMPERS                                                       
         LA    R6,2(R6)            FIRST PERIOD NAME                            
*                                                                               
         XC    SVPERIOD,SVPERIOD   TABLE OF SAVED PERIODS                       
         LA    R3,SVPERIOD                                                      
PR30     MVC   0(4,R3),1(R6)       SAVE EACH NAME IN TABLE                      
         LA    R3,4(R3)                                                         
         LA    R6,5(R6)                                                         
         BCT   R1,PR30                                                          
*                                                                               
         MVC   KEY(13),SVKEY       RESTORE STATION UPGRADE KEY                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RESTORE DMGR SEQUENCE                        
         CLC   SCHEME,KEY+2                                                     
         BE    PR50                                                             
*                                                                               
         XC    TMAR,TMAR                                                        
         XC    TSTA,TSTA                                                        
         MVI   FIRSTIME,C'Y'                                                    
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE ON SCHEME CHANGE              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   HDHOOKOK,C'Y'                                                    
         MVC   SCHEME,KEY+2                                                     
*                                                                               
         MVI   REPMUSER,C'N'       ASSUME USER DOESN'T USE REP MARKETS          
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         OC    SCHEME,SCHEME                                                    
         BZ    PR40                                                             
         GOTO1 CLUNPK,DMCB,SCHEME,WORK+23                                       
PR40     GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+10                NO                                           
         MVC   REPMUSER,WORK+1                                                  
*                                                                               
PR50     MVI   RECFOUND,C'Y'       FOUND RECORD STATION TYPE                    
         CLI   FIRSTIME,C'Y'                                                    
         BNE   PR60                                                             
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR60     XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',SIRKMS),QMKT,WORK                             
*                                                                               
         CLC   TMAR,QMKT           TEST IF CHANGE OF MARKET                     
         BE    PR70                                                             
         CLI   FIRSTIME,C'Y'                                                    
         BE    PR70                                                             
         MVI   ALLOWLIN,2          IF NOT SAME MARKET, INSERT LINE              
         XC    TSTA,TSTA                                                        
*                                                                               
         OC    ABOX,ABOX                                                        
         BZ    PR70                                                             
         L     R5,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R5                                                               
*                                                                               
PR70     MVC   SVKEY(13),KEY       SAVE STATION UPGRADE KEY                     
         CLC   QMKT,TMAR           ONLY PRINT MARKET ONCE                       
         BE    PR140                                                            
*                                                                               
         CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   PR120               NO                                           
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   PR80                                                             
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         MVC   SVUTLSYS,4(R5)      SAVE SPOT SENUM                              
         MVI   4(R5),X'08'         PUT REP SENUM IN UTL                         
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO2                    
         B     PR90                                                             
*                                                                               
PR80     GOTO1 SWITCH,DMCB,=C'REP',0                                            
         CLI   DMCB+4,2            TEST REP SYSTEM IS STARTED                   
         BNE   *+12                YES                                          
         MVI   ERROR,REPISOFF                                                   
         B     TRAPERR                                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
PR90     LA    R5,KEY              READ MARKET KEY                              
         USING RMKTRECD,R5                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,QMKT                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0             TEST VALID MARKET                            
         BE    *+14                YES                                          
         MVC   PRMKTNM,=CL24'*** UNKNOWN ***'                                   
         B     PR100                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'REPFIL',KEY+28,          +        
               AIO2,DMWORK                                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO2             DISPLAY MARKET NAME                          
         MVC   PRMKTNM(L'RMKTNAME),RMKTNAME                                     
         MVC   PRMKT,QMKT                                                       
         DROP  R5                                                               
*                                                                               
PR100    CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   PR110                                                            
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'REP'                                  
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         MVC   4(1,R5),SVUTLSYS    SWITCH BACK TO SPOT SYSTEM                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',SPTFLIST,AIO2                   
         B     PR140                                                            
*                                                                               
PR110    GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    PR140               YES                                          
         DC    H'0'                                                             
*                                                                               
PR120    MVI   KEY,C' '            BUILD MARKET RECORD KEY                      
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVI   KEY+1,C'T'          MEDIA 'T'                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO2                                                          
         USING MKTRECD,R5                                                       
         CLC   KEY(8),0(R5)        DO WE NEED TO READ THE RECORD                
         BE    PR130               NO, WE HAVE ALREADY READ IT                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO2                 
PR130    LA    R1,=CL24'UNKNOWN'                                                
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   PRMKTNM,0(R1)                                                    
         MVC   PRMKT,QMKT                                                       
         DROP  R5                                                               
*                                                                               
PR140    MVC   TMAR,QMKT                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       RESTORE STATION UPGRADE KEY                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RESTORE DMGR SEQUENCE                        
         CLC   TSTA,WORK           ONLY PRINT STATION ONCE                      
         BE    PR150                                                            
         OC    SIRKSTA,SIRKSTA     TEST ANY STATION                             
         BNZ   *+14                                                             
         MVC   PRSTA,=CL8'ALL'                                                  
         B     PR150                                                            
         MVC   PRSTA,WORK          STATION                                      
         CLI   PRSTA,X'F0'         CABLE HEADEND?                               
         BL    PR150                                                            
         MVI   PRSTA+4,C'/'        YES                                          
*                                                                               
PR150    MVC   TSTA,WORK                                                        
         LA    R3,SVPERIOD         BEGINNING OF PERIOD TABLE                    
         SH    R3,=H'4'            BACK UP TO BEFORE TABLE START                
         MVC   BYTE,SIRKMON                                                     
         NI    BYTE,X'FF'-SIRKBUYQ                                              
         ZIC   R0,BYTE                                                          
         LA    R3,4(R3)            FIND NAME IN TABLE                           
         BCT   R0,*-4                                                           
         MVC   PRPER,0(R3)         PERIOD NAME                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         USING EUPELEM,R6                                                       
         MVI   WORK,C' '           BUILD UPGRADE EXPRESSION                     
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EUPUPFIL                                               
*                                                                               
         CLI   EUPUPGRD,SPUPTPUT   TEST HUT OR PUT                              
         BNE   PR160               NO, NEITHER ONE                              
         CLI   EUPUPGRD+1,C'P'     TEST PUT                                     
         BNE   PR160               NO, IT'S A HUT                               
*                                                                               
         MVC   WORK+4(4),=C'PUT/'                                               
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPGRD+2     RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPGRD+2                                                    
         GOTO1 DATCON,DMCB,(3,EUPUPGRD+2),(6,WORK+8)                            
         MVC   WORK+11(2),WORK+12  GET RID OF '/'                               
         MVI   WORK+13,C' '                                                     
         B     *+10                                                             
*                                                                               
PR160    MVC   WORK+4(16),EUPUPINP                                              
*                                                                               
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    PR170                                                            
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPFBK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPFBK                                                      
*                                                                               
         LA    R5,WORK+21                                                       
         CLI   0(R5),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','          EDIT IN COMMA                                
         LA    R5,2(R5)                                                         
         MVC   0(3,R5),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R5))                               
*                                                                               
PR170    MVC   PRUPG,WORK                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE ON REPORT                         
         MVI   FIRSTIME,C'N'                                                    
         B     PR10                                                             
*                                                                               
PR180    CLI   RECFOUND,C'Y'       TEST REPORT HAS DATA IN IT                   
         BE    PRX                 YES                                          
         MVI   HDHOOKOK,C'N'                                                    
         MVC   H6(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                                                                   
*                                                                               
         CLI   HDHOOKOK,C'N'       TEST OK TO DO HEADHOOK                       
         BE    HOOKX               NO                                           
*                                                                               
         MVI   H3,0                SKIP A LINE                                  
         MVC   H4(6),=C'SCHEME'                                                 
         OC    SIRKCODE,SIRKCODE                                                
         BNZ   *+14                                                             
         MVC   H4+10(3),=C'ALL'                                                 
         B     HOOK10                                                           
         GOTO1 CLUNPK,DMCB,SIRKCODE,H4+10                                       
*                                                                               
HOOK10   MVI   H7,0                                                             
         MVC   PRMKT+8(6),=C'MARKET'                                            
         MVC   PRSTA,=C'STATION '                                               
         MVC   PRPER(6),=C'PERIOD'                                              
         MVC   PRUPG+11(7),=C'UPGRADE'                                          
*                                                                               
         OC    ABOX,ABOX           TEST WE HAVE BOXES                           
         BZ    HOOKX                                                            
*                                                                               
         L     R4,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+10,C'T'                                                  
         MVI   BOXROWS+55,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+23,C'L'                                                  
         MVI   BOXCOLS+63,C'C'                                                  
         MVI   BOXCOLS+72,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+114,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
HEDSPECS SSPEC H1,1,C'MEDIA     SPOT T.V.'                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,51,C'STATION DEFAULT UPGRADE REPORT'                          
         SSPEC H2,51,C'------------------------------'                          
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         DC    X'00'                                                            
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMBDD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
NOTAUTH  EQU   0175                                                             
NUMPERS  DS    H                   TOTAL NUMBER OF PERIODS                      
SCHEME   DS    XL2                                                              
PERNUM   DS    X                   PERIOD NUMBER IN KEY                         
CURYEAR  DS    X                                                                
MYWORK   DS    CL64                                                             
SAVEKEY  DS    XL13                                                             
SVPERIOD DS    CL48                TABLE OF PERIOD NAMES                        
HDHOOKOK DS    C                                                                
RECFOUND DS    C                                                                
FIRSTIME DS    C                                                                
REPMUSER DS    C                   'Y' IF WE'RE READING REP MARKETS             
SVUTLSYS DS    C                   SAVED SENUM BEFORE FASWITCH                  
TMAR     DS    CL4                                                              
TSTA     DS    CL8                                                              
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE SPDEMUPD                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
RMKTRECD DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
         PRINT ON                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTMKT   DS    CL4                                                              
         DS    CL1                                                              
LSTMKTNM DS    CL24                                                             
         DS    CL1                                                              
LSTSTA   DS    CL8                                                              
         DS    CL2                                                              
LSTPER   DS    CL4                                                              
         DS    CL1                                                              
LSTUPG   DS    CL29                                                             
         SPACE 3                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL30                                                             
PRMKT    DS    CL4                                                              
         DS    CL4                                                              
PRMKTNM  DS    CL24                                                             
         DS    CL2                                                              
PRSTA    DS    CL8                                                              
         DS    CL1                                                              
PRPER    DS    CL4                                                              
         DS    CL10                                                             
PRUPG    DS    CL33                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040SPSFM0D   05/01/02'                                      
         END                                                                    
