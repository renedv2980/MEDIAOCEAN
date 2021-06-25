*          DATA SET TASYSVAL   AT LEVEL 013 AS OF 10/30/19                      
*PHASE T00A87B,*                                                                
*INCLUDE TALDCPTR                                                               
*INCLUDE TAYTD                                                                  
*INCLUDE TINVCON                                                                
         TITLE 'TASYSVAL (T00A87) - TALENT SYSTEM COMMON ROUTINES'              
TASYSVL  CSECT                                                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
         USING *,RF                                                             
TVAL     NTR1                                                                   
         DROP  RF                                                               
         LR    RB,RF                                                            
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING TASYSVL,RB,R7,R6                                                 
         B     *+12                                                             
         DC    CL8'TASYSVL'                                                     
         USING GEND,RC             RC=A(GENCON AREAS)                           
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL AREAS)                            
         L     R9,ASTARTSV                                                      
         USING SYSWORKD,R9         R9=A(GLOBAL SAVED VALUES)                    
         ST    RD,TGRD                                                          
         RELOC TGRELOV                                                          
         SPACE 1                                                                
         SRL   RF,24                                                            
         LA    RF,ADDRTAB(RF)                                                   
         L     RF,0(RF)            RF=A(ROUTINE)                                
         A     RF,TGRELOV                                                       
*                                                                               
         L     RE,=A(SYSVAL2)      IS ROUTINE IN MAIN SECTION                   
         A     RE,TGRELOV                                                       
         SLL   RE,8                                                             
         SRL   RE,8                                                             
         SLL   RF,8                                                             
         SRL   RF,8                                                             
         CR    RF,RE                                                            
         BLR   RF                  YES, SO GO THERE                             
         BR    RE                  NO,  GO TO SYSVAL2                           
         SPACE 1                                                                
ADDRTAB  DS    0A                                                               
         DC    AL4(CHAROUT)        X'00'                                        
         DC    AL4(NAMIN)          X'04'                                        
         DC    AL4(ADDRIN)         X'08'                                        
         DC    AL4(ACTVOUT)        X'0C'                                        
         DC    AL4(USERVAL)        X'10'                                        
         DC    AL4(RECVAL)         X'14'                                        
         DC    AL4(XNAME)          X'18'                                        
         DC    AL4(ACTVIN)         X'1C'                                        
         DC    AL4(CATVAL)         X'20' - SYSVAL2                              
         DC    AL4(USEVAL)         X'24'                                        
         DC    AL4(GETL)           X'28'                                        
         DC    AL4(DELL)           X'2C'                                        
         DC    AL4(UNIVAL)         X'30' - SYSVAL2                              
         DC    AL4(YRVAL)          X'34' - SYSVAL2                              
         DC    AL4(UPGRVAL)        X'38' - SYSVAL2                              
         DC    AL4(MAJVAL)         X'3C' - SYSVAL2                              
         DC    AL4(MEDVAL)         X'40'                                        
         DC    AL4(EXTRACT)        X'44'                                        
         DC    AL4(TAXVAL)         X'48' - SYSVAL2                              
         DC    AL4(CCTYPVAL)       X'4C'                                        
         DC    AL4(CTYPVAL)        X'50'                                        
         DC    AL4(GETOV1)         X'54'                                        
         DC    AL4(ADDL)           X'58'                                        
         DC    AL4(BTYPVAL)        X'5C' - SYSVAL2                              
         DC    AL4(APPLVAL)        X'60' - SYSVAL2                              
         DC    AL4(TRACE)          X'64'                                        
         DC    AL4(SYSVINIT)       X'68'                                        
         DC    AL4(ERROUT)         X'6C' - SYSVAL2                              
         DC    AL4(PDVAL)          X'70'                                        
         DC    AL4(STAFVAL)        X'74'                                        
         DC    AL4(BLDTRK)         X'78'                                        
         DC    AL4(GUARVAL)        X'7C'                                        
         DC    AL4(EXPIRE)         X'80'                                        
         DC    AL4(PRTSCRN)        X'84'                                        
         DC    AL4(LICVAL)         X'88' - SYSVAL2                              
         DC    AL4(SAVPTRS)        X'8C'                                        
         DC    AL4(ADDPTRS)        X'90'                                        
         DC    AL4(GENPTRS)        X'94'                                        
         DC    AL4(ADJOUT)         X'98' - SYSVAL2                              
         DC    AL4(CERROUT)        X'9C' - SYSVAL2                              
         DC    AL4(DTVAL)          X'A0'                                        
         DC    AL4(CONCLUDE)       X'A4'                                        
         DC    AL4(CHNINV)         X'A8'                                        
         DC    AL4(GETTHRES)       X'AC'                                        
         DC    AL4(PWRDVAL)        X'B0' - SYSVAL2                              
         DC    AL4(CHNADV)         X'B4'                                        
         DC    AL4(TIMECON)        X'B8' - SYSVAL2                              
         DC    AL4(VALTYP)         X'BC' - SYSVAL2                              
         DC    AL4(UNITEST)        X'C0' - SYSVAL2                              
         DC    AL4(SSNPACK)        X'C4' - SYSVAL2                              
         DC    AL4(SSNUNPK)        X'C8' - SYSVAL2                              
         DC    AL4(ADDERROR)       X'CC' - SYSVAL2                              
         DC    AL4(VALCTRY)        X'D0' - SYSVAL2                              
         DC    AL4(TRNSAGT)        X'D4' - SYSVAL2                              
         DC    AL4(SETOV2)         X'D8' - SYSVAL2                              
         DC    AL4(TSTLCKT)        X'DC' - SYSVAL2                              
         DC    AL4(SETLCKT)        X'E0' - SYSVAL2                              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SYSTEM INITIALIZATION ROUTINES                                   
         SPACE 1                                                                
SYSVINIT DS    0H                                                               
         LM    R2,R3,0(R1)         R2=A(STAFF ID), R3=A(PASSWORD)               
*                                  *NEED THESE REGS FOR VALSTAFF CALL*          
         SPACE 1                                                                
         L     R1,=V(LDCPTR)       SAVE A(CREATE PASSIVE PTR MODULE)            
         A     R1,TGRELOV                                                       
         ST    R1,TGTALDCP                                                      
         L     R1,=V(TAYTD)        SAVE A(YTD MODULE)                           
         A     R1,TGRELOV                                                       
         ST    R1,TGTAYTD                                                       
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF NOT OFFLINE                               
         BE    SYSV00                                                           
         CLI   TWAFIRST,0          OR NOT VERY FIRST TIME                       
         BE    SYSV10                                                           
         CLC   SYSDIR,=CL8'TALDIR'                                              
         BE    SYSV25              THEN SKIP AHEAD TO SYS REC READ              
         B     XIT                                                              
         SPACE 1                                                                
SYSV00   GOTO1 DATVAL,DMCB,(0,RCDATE),TGTODAY0 TODAY'S RUN DATES                
         SPACE 1                                                                
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         CLI   MCRECOVR,C'Y'       IF WE'RE RECOVERING                          
         BE    *+12                                                             
         CLI   MCRECOVR,C'W'                                                    
         BNE   *+12                                                             
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   SET RECOVER COPIES                           
         B     SYSV20                                                           
         SPACE 1                                                                
SYSV10   GOTO1 DATCON,DMCB,(5,0),(0,TGTODAY0)  TODAY'S DATES                    
         SPACE 1                                                                
SYSV20   GOTO1 DATCON,DMCB,(0,TGTODAY0),(1,TGTODAY1)                            
         GOTO1 (RF),(R1),,(2,TGTODAY2)                                          
         GOTO1 (RF),(R1),,(8,TGTODAY8)                                          
         GOTO1 (RF),(R1),,(20,TGTODY20)                                         
         SPACE 1                                                                
         BAS   RE,VALSTAFF         VALIDATE STAFF ID  (PASS R2,R3)              
*                                  GET NEXT BUSINESS DAY                        
         LA    R1,DMCB                                                          
         LA    RE,TGTODAY1                                                      
         ST    RE,0(R1)                                                         
         MVI   0(R1),1                                                          
         BRAS  RE,GTNXTBUS                                                      
         LA    R3,WORK             R3=A(GETRET BLOCK)                           
         USING GETRETD,R3                                                       
         GOTO1 DATCON,DMCB,(3,GRDODY),(1,TGNXTBUS)  SAVE IN GLOBAL W/S          
*                                  GET NEXT BUSINESS DAY+1                      
         LA    R1,DMCB                                                          
         LA    RE,TGNXTBUS                                                      
         ST    RE,0(R1)                                                         
         MVI   0(R1),1                                                          
         BRAS  RE,GTNXTBUS                                                      
         LA    R3,WORK             R3=A(GETRET BLOCK)                           
         USING GETRETD,R3                                                       
         GOTO1 DATCON,DMCB,(3,GRDODY),(1,TGNXT2DY)  SAVE IN GLOBAL W/S          
*                                                                               
         MVC   TGCTALPH,TWAAGY     SAVE CONNECT ALPHA CODE                      
         MVC   TGUSER,TWAORIG      AND USERID                                   
         SPACE 1                                                                
SYSV25   GOTO1 RECVALL,DMCB,TLSYCDQ,(X'20',0)  GET SYSTEM RECORD                
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TASYELQ      LOOK UP SYSTEM CONTROL ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASYD,R3                                                         
         MVC   TGTPEMP,TASYEMP     SAVE DEFAULT EMPLOYER                        
         MVC   TGUSBNK,TASYUSBK         US BANK ACCOUNT                         
         MVC   TGCNBNK,TASYCNBK         CAN. BANK ACCOUNT                       
         MVC   TGSYSTAT,TASYSTAT        SYSTEM STATUS                           
         MVC   TGSYSTA2,TASYSTA2        SYSTEM STATUS 2                         
         MVC   TGSYITCD,TASYITCD        INSTALL TIMING CHG LIVE DATE            
         MVC   TGLCBNK,TASYLCBK         PAYROLL PLUS BANK ACCOUNT               
         SPACE 1                                                                
***********************************************************************         
* IF ONLINE AND CHECK LOCKOUT DATE IS SET BUT CHECK LOCKOUT IS ACTIVE *         
* TURN OFF CHECK LOCKOUT                                              *         
***********************************************************************         
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    SYSV40                                                           
         OC    TASYLKEN,TASYLKEN                                                
         BZ    SYSV27                                                           
         GOTOR TSTLCKTA,DMCB,=C'TAL_CHECKS'                                     
         BNE   SYSV27                                                           
         GOTOR SETLCKTA,DMCB,=C'TAL_CHECKS',0                                   
                                                                                
SYSV27   OC    TASYPLEN,TASYPLEN                                                
         BZ    SYSV28                                                           
         GOTOR TSTLCKTA,DMCB,=C'TAL_PRCHKS'                                     
         BNE   SYSV28                                                           
         GOTOR SETLCKTA,DMCB,=C'TAL_PRCHKS',0                                   
                                                                                
SYSV28   OC    TASYLLEN,TASYLLEN                                                
         BZ    SYSV30                                                           
         GOTOR TSTLCKTA,DMCB,=C'TAL_P+CHKS'                                     
         BNE   SYSV30                                                           
         GOTOR SETLCKTA,DMCB,=C'TAL_P+CHKS',0                                   
                                                                                
SYSV30   CLI   TWAFIRST,0                                                       
         BNE   SYSVX                                                            
         DROP  R3                                                               
         SPACE 1                                                                
SYSV40   BAS   RE,BLDLIMIT         BUILD LIMIT ACCESS LIST                      
         SPACE 1                                                                
         GOTO1 =A(BLDHWLCL),RR=TGRELOV BLD LIST OF AFM LOCS W/H&W FUNDS         
         SPACE 1                                                                
SYSVX    CLI   TGCTSTTY,TASTTYPP   IF PROGRAMMER                                
         BNE   *+8                                                              
         NI    GENSTAT5,ALL-NODLST  OK TO DELETE FROM LIST USING 'D'            
         SPACE 1                                                                
         BAS   RE,BLDRACT          SET SECURITY BITS IN RECACT TABLE            
         SPACE 1                                                                
         BAS   RE,CHKEXP           CHK PASSWORD - SET CC CODE & TGDUB           
         SPACE 1                                                                
         BRAS  RE,BLDS2LIM         SAVE STAFF2 LIMITS                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE STAFF ID AND PASSWORD                        
         SPACE 1                                                                
*                                  R2=A(STAFF FLD), R3=A(PASSWORD FLD)          
VALSTAFF NTR1                                                                   
         LR    R4,R3               R4=A(PASSWORD FIELD)                         
         SPACE 1                                                                
         MVI   BYTE,X'20'          SET TO DO GETREC                             
         BAS   RE,GETSYS           GET SYSTEM RECORD                            
         BNE   VST2                                                             
         L     R3,TGELEM           RETURNS CC EQ IF FOUND ACCESS EL.            
         USING TAAVD,R3                                                         
         CLI   TAAVERRS,TAAVEMAX   TEST EXCEEDED MAX. ALLOWED ERRORS            
         BH    VIOLATER            TERM IN VIOLATION - DISALLOW ACCESS          
         SPACE 1                                                                
VST2     CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   VST4                                                             
         L     R2,EFHKEY           STAFF ID SHOULD BE IN KEY FIELD              
         CLI   5(R2),0                                                          
         BE    VSTX                                                             
         MVC   CONSERV(5),=C'STAFF'  DISPLAY REQUESTOR                          
         MVC   CONSERV+7(8),8(R2)                                               
         SPACE 1                                                                
VST4     GOTO1 RECVALL,DMCB,TLSTCDQ,(X'A4',TGCTSTAF)                            
         BE    VST5A                                                            
         SPACE 1                                                                
         GOTO1 ANY                 REQUIRE STAFF CODE                           
         MVC   TGUSER,TWAORIG      SET CONNECT USERID                           
         SPACE 1                                                                
*&&DO                                                                           
         MVC   DUB,=X'C440C140D540C4C1'                                         
         MVC   DUB+1(1),TGTODAY0+3                                              
         MVC   DUB+3(1),TGTODAY0+4                                              
         MVC   DUB+5(1),TGTODAY0+5                                              
         CLC   DUB,8(R4)           TEST SPECIAL PASSWORD INPUT                  
         BNE   VST5                                                             
         MVC   TGCTSTAF,WORK       SET STAFF ID                                 
         MVI   TGCTSTTY,TASTTYPP   SET USER IS PROGRAMMER                       
         B     VST10               SKIP RECORD VAL - GO SET SECURITY            
*&&                                                                             
         SPACE 1                                                                
VST5     GOTO1 RECVALL,DMCB,TLSTCDQ,(X'24',(R2)) VALIDATE STAFF                 
         BNE   VSTNO               RECORD NOT FOUND                             
         SPACE 1                                                                
VST5A    L     R3,AIO                                                           
         MVI   ELCODE,TASTELQ      GET STAFF ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASTD,R3                                                         
         OC    KEY+TLSTUSER-TLSTD(L'TLSTUSER),KEYSAVE+TLSTUSER-TLSTD            
         BZ    *+12                                                             
         CLI   OFFLINE,C'Y'        IF WE'RE OFFLINE                             
         BNE   VST6                                                             
         XC    8(8,R2),8(R2)       CLEAR KEY FIELD                              
         MVI   5(R2),0                                                          
         B     VST8                AND DON'T BOTHER WITH PASSWORD               
         SPACE 1                                                                
VST6     LR    R2,R4               PASSWORD IS REQUIRED                         
         GOTO1 ANY                                                              
         XC    8(8,R2),8(R2)       CLEAR FIELD IN CASE ERROR MADE               
         OI    6(R2),X'80'                                                      
         MVI   ERROR,ERINVPSW      INVALID PASSWORD                             
         CLC   TASTPWD,WORK        TEST PASSWORD IS CORRECT                     
         BNE   VSTNO                                                            
         SPACE 1                                                                
VST8     MVC   TGCTSTAF,TGSTAF     SAVE CONNECT STAFF                           
         MVC   TGCTSTTY,TASTTYPE   AND CONNECT STAFF TYPE                       
         SPACE 1                                                                
VST10    GOTO1 STAFVALL,DMCB,TGCTSTTY,0  LOOK UP STAFF TYPE                     
         SPACE 1                                                                
         MVC   TGCTSTLV,TGSTLVL    SAVE CONNECT ACCESS LEVEL                    
         ZIC   R1,TGSTDSP          DISPLACEMENT TO MASK BYTE IN TWA0            
         LA    R1,SECMASKS(R1)                                                  
         MVC   0(1,R1),TGSTBIT     SAVE MASK BIT                                
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPC   IS THIS PERSON A CLIENT                      
         BE    VST20                                                            
         CLI   TGCTSTTY,TASTTYPD                                                
         BE    VST20                                                            
         CLI   TGCTSTTY,TASTTYPF                                                
         BNE   *+8                                                              
         SPACE 1                                                                
VST20    OI    TGCTSTST,TGCTSCLI   SET BIT                                      
         SPACE 1                                                                
         BAS   RE,VIOLOUT          SUCCESSFULLY CONNECTED - DEL VIOL EL         
         SPACE 1                                                                
VSTX     B     XIT                                                              
         SPACE 1                                                                
VSTNO    IC    R1,ERROR            SAVE CURRENT ERROR                           
         BAS   RE,VIOLIN           ADD ACCESS VIOLATION ELEMENT                 
         BH    VIOLATER            MAX. ERRORS EXCEEDED - VIOLATION             
         STC   R1,ERROR            ELSE RESTORE ORIGINAL ERROR                  
         B     THEEND              AND RETURN TO USER                           
         EJECT                                                                  
*              ROUTINE TO ADD ACCESS VIOLATION ELEMENTS                         
         SPACE 1                                                                
VIOLIN   NTR1                                                                   
         MVI   BYTE,X'30'          SET TO DO GETREC & READ FOR UPDATE           
         BAS   RE,GETSYS           GET SYSTEM RECORD                            
         BNE   *+12                                                             
         L     R3,TGELEM           FOUND ELEMENT, SO USE IT                     
         B     VI4                                                              
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     ELSE BUILD ACCESS VIOLATION ELEMENT          
         LA    R3,ELEMENT                                                       
         USING TAAVD,R3                                                         
         MVI   TAAVEL,TAAVELQ      ELEMENT CODE                                 
         MVI   TAAVLEN,TAAVLNQ     ELEMENT LENGTH                               
         MVC   TAAVLINE(8),TGLINEID LINE-ID/ADDRESS                             
         SPACE 1                                                                
VI4      ZIC   R1,TAAVERRS         BUMP N'ERRORS                                
         AHI   R1,1                                                             
         STC   R1,TAAVERRS                                                      
         SPACE 1                                                                
         GOTOR TSTLCKTA,DMCB,=C'TAL_CHECKS'                                     
         BE    VIX                 SKIP AHEAD IF CHECK LOCKOUT ACTIVE           
         GOTOR TSTLCKTA,DMCB,=C'TAL_PRCHKS'                                     
         BE    VIX                 SKIP AHEAD IF CHECK LOCKOUT ACTIVE           
         GOTOR TSTLCKTA,DMCB,=C'TAL_P+CHKS'                                     
         BE    VIX                 SKIP AHEAD IF CHECK LOCKOUT ACTIVE           
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(5,0),(1,TAAVDATE)  TODAY'S DATE                     
         TIME  DEC                                                              
         STCM  R0,14,TAAVTIME      CURRENT TIME                                 
         CLI   TAAVLEN,TAAVLNQ     IS THIS THE NEW ELEMENT LENGTH?              
         BNE   *+16                IF NOT, DO NOT ENTER STAFF ID                
         MVC   TAAVSTAF,TGSTAF     STAFF ID                                     
         MVC   TAAVUSER,TGUSER     USER ID NUMBER                               
         SPACE 1                                                                
         CLI   TGELEM,0            TEST ELEMENT ALREADY ON RECORD               
         BE    VI6                                                              
         GOTO1 ADDELEM             NO - ADD ELEMENT TO RECORD                   
         SPACE 1                                                                
VI6      CLI   OFFLINE,C'Y'        DIE IF OFFLINE                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         GOTO1 PUTREC              ELSE, WRITE BACK THE RECORD                  
         SPACE 1                                                                
VIX      CLI   TAAVERRS,TAAVEMAX   RETURN CC HIGH IF VIOLATION                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET SYSTEM RECORD (AND ACCESS VIOLATION EL.)          
         SPACE 1                                                                
GETSYS   NTR1                                                                   
         TM    BYTE,X'10'                                                       
         BZ    GSYS10                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    GSYS10                                                           
         GOTOR TSTLCKTA,DMCB,=C'TAL_CHECKS'                                     
         BE    NO                                                               
         GOTOR TSTLCKTA,DMCB,=C'TAL_PRCHKS'                                     
         BE    NO                                                               
         GOTOR TSTLCKTA,DMCB,=C'TAL_P+CHKS'                                     
         BE    NO                                                               
         SPACE 1                                                                
GSYS10   GOTO1 RECVALL,DMCB,TLSYCDQ,(BYTE,0) GET SYSTEM RECORD                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVI   ELCODE,TAAVELQ                                                   
         GOTO1 GETLL,DMCB,(8,TGLINEID)  GET EXISTING EL. FOR THIS TERM          
         SPACE 1                                                                
         B     XIT                  GETL RETURNS CC                             
         SPACE 3                                                                
*              ROUTINE TO DELETE ACCESS VIOLATION ELEMENTS                      
         SPACE 1                                                                
VIOLOUT  NTR1                                                                   
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BE    XIT                 OR CHECK LOCKOUT ACTIVE                      
         GOTOR TSTLCKTA,DMCB,=C'TAL_CHECKS'                                     
         BE    XIT                 DON'T CARE ABOUT DELETING ELEMENT            
         GOTOR TSTLCKTA,DMCB,=C'TAL_PRCHKS'                                     
         BE    XIT                 DON'T CARE ABOUT DELETING ELEMENT            
         GOTOR TSTLCKTA,DMCB,=C'TAL_P+CHKS'                                     
         BE    XIT                 DON'T CARE ABOUT DELETING ELEMENT            
         SPACE 1                                                                
         MVI   BYTE,X'30'          SET TO DO GETREC & READ FOR UPDATE           
         BAS   RE,GETSYS           GET SYSTEM RECORD                            
         BNE   XIT                 DON'T BOTHER IF NOT AROUND                   
         SPACE 1                                                                
         GOTO1 DELLL,DMCB,(8,TGLINEID) DELETE EL. FOR THIS TERMINAL             
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK THE RECORD                        
         B     XIT                                                              
         EJECT                                                                  
*              BUILD LISTS OF AGENCY AND CLIENT LIMIT ACCESS CODES              
*              AND SET ADDITIONAL CLIENT GROUP ACCESS                           
         SPACE 1                                                                
BLDLIMIT NTR1                                                                   
         CLI   TGCTSTAF,0          DON'T BOTHER IF DON'T HAVE STAFF ID          
         BE    BLIMX                                                            
         GOTO1 RECVALL,DMCB,TLSTCDQ,(X'A0',TGCTSTAF)  GET STAFF REC.            
         BE    *+14                                                             
         CLI   TGCTSTTY,TASTTYPP   NOT FOUND OK IF PROGRAMMER                   
         BE    BLIMX                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
         USING TAVAD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TAVAELQ      LOOK FOR AGENCY/CLIENT LIMIT ELEMENT         
         BAS   RE,GETEL                                                         
         BNE   BLIM2                                                            
         MVC   TGAGY,TAVAAGY       MOVE FIRST AGENCY TO GLOBAL NOW              
         DROP  R3                                                               
         SPACE 1                                                                
BLIM2    L     R3,AIO                                                           
         MVI   ELCODE,TASTELQ      LOOK FOR CLIENT GROUP ACCESS                 
         BAS   RE,GETEL                                                         
         BNE   BLIMX                                                            
         USING TASTD,R3                                                         
         MVC   TGCLGACC,TASTCLG    SAVE CLIENT GROUP ACCESS                     
         MVC   TGCLG,TASTCLG       MOVE THE CLIENT GROUP TO GLOBAL              
BLIMX    B     XIT                                                              
         EJECT                                                                  
*              SET SECURITY BYTES IN RECACT TABLE                               
         SPACE 1                                                                
BLDRACT  NTR1                                                                   
         CLI   OFFLINE,C'Y'        DON'T BOTHER IF OFFLINE                      
         BE    XIT                                                              
         CLC   TGUSER,=H'7538'     OR IF USER-ID IS TALFQA                      
         BE    XIT                                                              
         CLC   TGUSER,=H'7697'     OR CLIFQA                                    
         BE    XIT                                                              
*                                                                               
         L     R2,ARECACT                                                       
         SHI   R2,RACTLNQ                                                       
         USING RACTD,R2            R2=A(RECACT TABLE HEADER)                    
*                                                                               
         CLI   RACTSTAT,RACTINIT   DOES TABLE NEED TO BE INITIALIZED            
         BNE   XIT                                                              
         GOTO1 PROTOFF             YES, TURN OFF STORAGE PROTECTION             
         XC    KEY,KEY             SET TO READ SECURITY RECORDS                 
         LA    R5,KEY                                                           
         USING TLSED,R5                                                         
         MVI   TLSECD,TLSECDQ      SET RECORD CODE                              
         MVC   TLSEPROG,SYSPHASE+2 PROGRAM NUMBER                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 HIGH                                                             
         B     BR30                                                             
*                                                                               
BR20     GOTO1 SEQ                                                              
*                                                                               
BR30     CLC   TLSEKEY(TLSEPROG-TLSED+L'TLSEPROG),KEYSAVE                       
         BNE   BR70                                                             
*                                                                               
         LA    R3,RACTTBL          R3=A(RECACT TABLE)                           
         OC    TLSEREC,TLSEREC     IF RECORD TYPE DEFINED                       
         BZ    BR40                                                             
         MVI   WORK,1              THEN LOOK UP RECORD TYPE                     
         MVC   WORK+1(8),TLSEREC                                                
         LA    R1,9-1              R1=L'EXECUTED COMPARE                        
         BAS   RE,LOOKUP                                                        
         BE    BR50                NOT IN TABLE - POSSIBLE ERROR                
*                                                                               
         MVC   BYTE,9(R3)          FOUND IT - SAVE RECORD NUMBER                
*                                                                               
         OC    TLSEACT,TLSEACT     IF ACTION NOT DEFINED                        
         BZ    BR60                THEN DONE - GO GET RECORD                    
*                                                                               
BR40     MVI   WORK,2              LOOK UP ACTION                               
         MVC   WORK+1(8),TLSEACT                                                
         LA    R1,9-1              R1=L'EXECUTED COMPARE                        
         BAS   RE,LOOKUP                                                        
         BE    BR50                NOT IN TABLE - POSSIBLE ERROR                
*                                                                               
         OC    TLSEREC,TLSEREC     IF RECORD NOT DEFINED                        
         BZ    BR60                THEN DONE - GO GET RECORD                    
*                                                                               
         MVI   WORK,3              LOOK UP RECORD/ACTION COMBINATION            
         MVC   WORK+1(1),BYTE                                                   
         MVC   WORK+2(1),10(R3)                                                 
         LA    R1,3-1                                                           
         BAS   RE,LOOKUP                                                        
         BNE   BR60                                                             
*                                                                               
BR50     TM    TLSEKEY+TLDRSTAT-TLDRD,X'80' NOT FOUND-SKIP IF DELETED           
         BO    BR20                                                             
         GOTO1 PROTON              TURN ON STORAGE PROTECTION                   
         B     RACTERR             ELSE GIVE ERROR                              
*                                                                               
BR60     GOTO1 GETREC              GET SECURITY RECORD                          
         LR    R4,R3               SAVE A(RECACT TABLE ENTRY)                   
         L     R3,AIO              (NEED R3 FOR GETEL)                          
*                                                                               
         XC    12(4,R4),12(R4)     PRE-CLEAR SECURITY MASK                      
         TM    TLSESTAT-TLSED(R3),X'80'  IF RECORD IS DELETED                   
         BO    BR20                      THEN LEAVE NO SECURITY                 
*                                                                               
         MVI   ELCODE,TASEELQ      GET SECURITY ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASED,R3                                                         
         MVC   12(4,R4),TASEMASK   SET MASK FROM ELEMENT                        
         B     BR20                GO GET NEXT SECURITY RECORD                  
*                                                                               
BR70     MVI   RACTSTAT,RACTOK     SET RECACT TABLE IS UPDATED                  
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
*                                                                               
         LA    RF,PSREC            FIND TASYSTAB IN DATASPACE                   
         USING PROGSPCD,RF                                                      
         XC    PSREC,PSREC                                                      
         MVC   PSSYS(4),=X'000A88E3'                                            
         DROP  RF                                                               
         GOTO1 PROTON              TURN ON STORAGE PROTECTION                   
*                                  TABLE REBUILT                                
         ICM   RF,7,=X'000A88'      TASYSTAB                                    
         ICM   RF,8,=C'S'                                                       
         GOTO1 CALLOV,DMCB,PSREC,(RF),(C'D',0)                                  
         CLC   PSREC(4),=X'000A88E3'   MAKE SURE IT'S THE SAME                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,PSREC                                                         
         ICM   RF,8,=C'W'                                                       
         GOTO1 CALLOV,DMCB,TGSYSTAB,(RF),0                                      
         CLI   4(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
PSREC    DS    XL(PROGSPCL)                                                     
         EJECT                                                                  
*              ROUTINE TO CHECK IF CURRENT PASSWORD HAS EXPIRED                 
*                                  XIT CC EQU - OLD PASSWORD OKAY               
*                                      CC NEQ - TGDUB=OLD PASSWORD              
         SPACE 1                                                                
CHKEXP   NTR1                                                                   
         CLI   OFFLINE,C'Y'        DON'T BOTHER IF OFFLINE                      
         BE    CHKEXPY                                                          
         CLI   TWAFIRST,0          DON'T BOTHER IF NOT FIRST TIME IN            
         BNE   CHKEXPY                                                          
         CLI   TGCTSTAF,0          DON'T BOTHER IF DON'T HAVE STAFF ID          
         BE    CHKEXPY                                                          
         GOTO1 RECVALL,DMCB,TLSTCDQ,(X'A0',TGCTSTAF)  GET STAFF REC.            
         BE    *+14                                                             
         CLI   TGCTSTTY,TASTTYPP   NOT FOUND OK IF PROGRAMMER                   
         BE    CHKEXPY                                                          
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASTD,R3                                                         
         MVC   TGDUB,TASTPWD       SAVE CURRENT PASSWORD (FOR CHANGE)           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,TAPWELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                  PASSWORD IS EXPIRED                          
         USING TAPWD,R3                                                         
*                                                                               
         MVC   WORK(3),TAPWDTE     DATE OF LAST CHANGE                          
         XC    WORK(3),HEXFFS      UNCOMPLEMENT IT                              
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
         GOTO1 ADDAY,DMCB,WORK+3,WORK+9,90                                      
         CLC   TGTODAY0,WORK+9     MUST CHANGE PASSWORD EVERY 90 DAYS           
         BL    CHKEXPY                                                          
         B     NO                  PASSWORD IS EXPIRED                          
*                                                                               
CHKEXPY  XC    TGDUB,TGDUB         PASSWORD NEED NOT BE CHANGED                 
         B     YES                                                              
         EJECT                                                                  
*              FINISHING UP ROUTINES                                            
         SPACE 1                                                                
CONCLUDE DS    0H                                                               
         L     R1,EFHKEY                                                        
         CLC   TGCTSTAF,8(R1)      IF STAFF ID STILL IN KEY FIELD               
         BNE   *+14                                                             
         XC    8(8,R1),8(R1)       CLEAR IT                                     
         MVI   5(R1),0                                                          
         SPACE 1                                                                
         TM    TGCTSTLV,X'C0'      ONLY FOR PROGRAMMERS/SYSTEM MANAGERS         
         BZ    *+12                                                             
         BAS   RE,DISPACCS         DISPLAY ACCESS CODES                         
         BRAS  RE,DISPLOCK         DISPLAY LOCKOUT STATUS                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY STAFF TYPES ACCESSED TO THIS REC/ACT          
         SPACE 1                                                                
DISPACCS NTR1                                                                   
         CLI   CONSERV+1,C'?'      IF 2ND BYTE OF SERVICE REQ. IS '?'           
         BNE   DACCX                                                            
         MVC   FULL,=4X'FF'        INIT. TO GLOBAL ACCESS                       
         L     R3,ARECACT                                                       
         MVI   WORK,1              FIRST LOOK UP RECORD TYPE                    
         L     RE,EFHREC                                                        
         MVC   WORK+1(8),8(RE)                                                  
         LA    R1,9-1              R1=L'EXECUTED COMPARE                        
         BAS   RE,LOOKUP                                                        
         BE    DACCX               GET OUT IF NOT FOUND                         
         MVC   BYTE,9(R3)          SAVE RECORD NUMBER                           
         SPACE 1                                                                
         MVI   WORK,2              NOW LOOK UP ACTION                           
         L     RE,EFHACT                                                        
         MVC   WORK+1(8),8(RE)                                                  
         BAS   RE,LOOKUP                                                        
         BE    DACCX               GET OUT IF NOT FOUND                         
         SPACE 1                                                                
         MVI   WORK,3                                                           
         MVC   WORK+1(1),BYTE      NOW LOOK UP RECORD/ACTION COMB.              
         MVC   WORK+2(1),10(R3)                                                 
         LA    R1,3-1                                                           
         BAS   RE,LOOKUP                                                        
         BE    DACCX               GET OUT IF NOT FOUND                         
         SPACE 1                                                                
         LA    R2,CONHED2          FIND FIRST AVAILABLE SLOT                    
         LA    R0,L'CONHED2-10                                                  
         CLI   0(R2),C' '          SCAN FOR FIRST OPEN SPOT                     
         BNH   *+16                                                             
         LA    R2,1(R2)            TRY NEXT                                     
         BCT   R0,*-16                                                          
         B     DACCX                                                            
         SPACE 1                                                                
         MVI   0(R2),C'('          START DISPLAY                                
         LA    R2,1(R2)                                                         
         GOTO1 HEXOUT,DMCB,3(R3),(R2),2,0  DISPLAY SCREEN/PROGRAM               
         MVI   4(R2),C','                                                       
         LA    R2,5(R2)                                                         
         SPACE 1                                                                
         L     R1,SYSPARMS         SET TO GET DISPLACEMENT TO CURSOR            
         L     R1,0(R1)                                                         
         USING TIOBD,R1                                                         
         LH    RF,TIOBCURD                                                      
         AR    RF,RA               RF=A(FIELD WHERE CURSOR IS)                  
         TM    1(RF),X'02'         IF FIELD HAS EXTENDED HEADER                 
         BZ    DACC4                                                            
         ZIC   R1,0(RF)                                                         
         AHI   R1,-8                                                            
         AR    RF,R1               RF=A(EXTENDED HEADER)                        
         EDIT  (1,0(RF)),(3,(R2)),ALIGN=LEFT  DISPLAY FIELD NUMBER              
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
DACC4    L     R4,TGASTAFS         LOOP THROUGH STAFF TYPE TABLE                
         USING STAFTABD,R4                                                      
DACC6    ZIC   R1,STAFDSP          DISPLACEMENT TO MASK BYTE                    
         LA    RF,FULL(R1)                                                      
         IC    R1,STAFBIT          BIT VALUE                                    
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(RF),0             IF BIT IS ON                                 
         BZ    *+14                                                             
         MVC   0(1,R2),STAFEQU     DISPLAY EQUATE CHARACTER                     
         LA    R2,1(R2)                                                         
         LA    R4,STAFNEXT                                                      
         CLI   0(R4),X'FF'         IF WE HAVEN'T REACHED END                    
         BNE   DACC6               CONTINUE                                     
         SPACE 1                                                                
         CLC   8(4,R3),SPACES      IF THERE ARE JCL CODES DEFINED               
         BE    DACC10                                                           
         MVI   0(R2),C','                                                       
         MVC   1(4,R2),8(R3)       DISPLAY THEM                                 
         LA    R2,5(R2)                                                         
         SPACE 1                                                                
DACC10   MVI   0(R2),C')'          END LITERAL                                  
DACCX    B     XIT                                                              
         SPACE 3                                                                
LOOKUP   DS    0H                  LOOK UP ROUTINE FOR RECACT TABLE             
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BER   RE                  RETURN CC EQ                                 
LOOK1    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R3)       MATCH ON SEARCH ARG. IN WORK                 
         BE    LOOK2                                                            
         CLI   WORK,1              IF LOOKING UP RECORDS                        
         BNE   *+12                                                             
         MVI   WORK,4              TRY FOR PROGREC                              
         B     LOOK1                                                            
         CLI   WORK,4              IF JUST CHECK PROGREC                        
         BNE   *+8                                                              
         MVI   WORK,1              RESTORE RECORD TYPE                          
         ZIC   R0,LRECACT                                                       
         AR    R3,R0               NO MATCH - TRY NEXT ENTRY IN TABLE           
         B     LOOKUP                                                           
         SPACE 1                                                                
LOOK2    CLC   12(4,R3),ZEROS      IF ACCESS DEFINED AT THIS LEVEL              
         BZ    *+10                                                             
         NC    FULL,12(R3)         'AND' WITH PREVIOUS LEVEL                    
         LTR   RE,RE               RETURN CC NE                                 
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE PERIOD                                       
         SPACE 1                                                                
*                                  P1=A(PERVAL OUTPUT BLOCK)                    
*                                     BYTE 0 X'80'=INPUT OPTIONAL               
*                                            X'40'=P2 HAS LEN,A(DATA)           
*                                            X'20'=DON'T RE-DISPLAY             
*                                            X'10'=VALIDATE AS MM/YY            
*                                            X'01'=INTERNAL USE                 
*                                  EITHER P2=A(FIELD)                           
*                                  OR R2=A(FIELD) (SEE X'40' BIT IN P1)         
PDVAL    DS    0H                                                               
         MVC   TGFULL,0(R1)        SAVE A(BLOCK)                                
         SPACE 1                                                                
         TM    0(R1),X'40'         IF DON'T HAVE A(FIELD)                       
         BZ    *+12                                                             
         L     R5,4(R1)            R5=L'DATA,A(DATA)                            
         B     PDV1                                                             
         SPACE 1                                                                
         LA    R5,8(R2)            R5=A(DATA)                                   
         ICM   R5,8,5(R2)          HOB=L'DATA                                   
         SPACE 1                                                                
PDV1     LR    R1,R5                                                            
         SRL   R1,24               R1=L'DATA                                    
         LTR   R1,R1                                                            
         BNZ   *+16                                                             
         TM    TGFULL,X'80'        TEST INPUT OPTIONAL                          
         BZ    FLDMISS                                                          
         B     PDV6                                                             
         LR    R4,R5               R4=A(DATA)                                   
         SPACE 1                                                                
         CLC   =C'TODAY-NEXTBDAY',0(R4)  IF SPECIAL LITERAL INPUT               
         BNE   *+8                                                              
         OI    TGFULL,X'01'              SET INTERNAL SWITCH FOR LATER          
         SPACE 1                                                                
PDV2     CLC   =C'NEXTBDAY',0(R4)  IMPLIES NEXT BUSINESS DAY                    
         BE    *+16                                                             
         LA    R4,1(R4)                                                         
         BCT   R1,PDV2                                                          
         B     PDV6                                                             
         SPACE 1                                                                
*        GOTO1 =A(GTNXTBUS),DMCB,(1,TGTODAY1),RR=TGRELOV                        
         LA    R1,DMCB                                                          
         LA    RE,TGTODAY1                                                      
         ST    RE,0(R1)                                                         
         MVI   0(R1),1                                                          
         BRAS  RE,GTNXTBUS                                                      
         LA    R3,WORK             R3=A(GETRET BLOCK)                           
         USING GETRETD,R3                                                       
         GOTO1 DATCON,DMCB,(3,GRDODY),(8,(R4))  MOVE DATE OVER LIT.             
         B     PDV1                             SCAN FOR MORE                   
         SPACE 2                                                                
PDV6     L     R3,TGFULL           R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         XC    PVALOUTB,PVALOUTB   INITIALIZE                                   
         LR    R4,R5                                                            
         SRL   R4,24               R4=L'DATA                                    
         LTR   R4,R4                                                            
         BZ    PDVX                                                             
         STC   R4,TGBYTE                                                        
         TM    TGFULL,X'10'        UNLESS SPECIFICALLY ASKED FOR MM/YY          
         BO    *+8                                                              
         OI    TGBYTE,X'40'        SET FLAG TO VALIDATE AS MM/DD                
         CLC   =C'NONE',0(R5)      SPECIAL - NO DATE                            
         BNE   *+12                                                             
         MVI   PVALPSTA,1                                                       
         B     PDVX                                                             
         CLC   =C'ANY',0(R5)       SPECIAL - ANY DATE                           
         BNE   PDV10                                                            
         MVI   PVALPSTA+2,1        SET START DATE TO 000001                     
         MVC   PVALPEND,=3X'FF'    AND END DATE TO FFFFFF                       
         B     PDVX                                                             
         SPACE 1                                                                
PDV10    MVC   PVALCSTA,TGTODAY2   PASS TODAY'S DATE                            
         SPACE 1                                                                
         GOTO1 PERVAL,DMCB,(TGBYTE,(R5)),('PVIN1DYL+PVINTOD',(R3))              
         SPACE 1                                                                
         TM    4(R1),PVRCINV1      TEST START DATE INVALID                      
         BZ    *+16                                                             
         TM    TGFULL,X'40'        IF HAVE A(FIELD)                             
         BZ    STRINV              GIVE ERROR                                   
         B     NO                  ELSE RETURN CC NE                            
         SPACE 1                                                                
         TM    4(R1),PVRCINV2      TEST END DATE INVALID                        
         BZ    *+16                                                             
         TM    TGFULL,X'40'        IF HAVE A(FIELD)                             
         BZ    ENDINV              GIVE ERROR                                   
         B     NO                  ELSE RETURN CC NE                            
         SPACE 1                                                                
         TM    4(R1),PVRCONE       IF BOTH DATES INPUT                          
         BO    PDV20                                                            
         CLI   4(R1),PVRCOK        THEN TEST GOOD RETURN CODE                   
         BNE   FLDINV                                                           
         B     PDV30                                                            
         SPACE 1                                                                
PDV20    MVC   PVALPEND,PVALPSTA   SET END DATE = START DATE                    
         SPACE 1                                                                
PDV30    TM    TGFULL,X'60'        IF DON'T HAVE FIELD OR SPEC. REQ'D           
         BNZ   PDVX                THEN DON'T RE-DISPLAY EXPANDED DATES         
         CLI   GCMODE,C'4'         IF HANDLING PROG RECS                        
         BNE   *+12                                                             
         CLI   OFFLINE,C'N'        AND NOT OFFLINE                              
         BE    PDVX                THEN DON'T RE-DISPLAY EXPANDED DATES         
         LR    R4,R2               SET TO PRE-CLEAR FIELD                       
         BAS   RE,CLEARFL2                                                      
         MVC   8(17,R2),PVALCPER   RE-DISPLAY GENERATED DATES                   
         MVI   5(R2),17                                                         
         TM    TGFULL,X'01'        IF SPECIAL BIT IS SET                        
         BZ    *+10                                                             
         MVC   8(8,R2),PVALCPER+9  DISPLAY END DATE IN START AS WELL            
         SPACE 1                                                                
PDVX     B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY CHARACTER DATA                                
         SPACE 1                                                                
*                                  P1 BYTE 0 X'80'=GET NAME IF NO SHORT         
*                                  P1 BYTE 3    = TANAELQ=NAME                  
*                                                 TASNELQ=SHORT NAME            
*                                                 TAFNELQ=FREEFORM NAME         
*                                                 TANUELQ=FREEFORM NUM          
*                                                 TACMELQ=COMMENT               
*                                                 TAADELQ=ADDRESS               
*                                  P2 BYTE  0   = N'FIELDS                      
*                                  P2 BYTES 1-3 = A(FIELD HEADER)               
*                                  P3 BYTE 3    = TYPE FOR TAFNELQ,             
*                                                      TANUELQ, TACMELQ         
*                                                                               
CHAROUTL NTR1  ,                   ENTRY POINT FOR LOCAL CALLS                  
CHAROUT  DS    0H                                                               
         XR    R4,R4                                                            
         ICM   R4,7,5(R1)          R4=A(FIELD HEADER) OR ZERO                   
         ZIC   R0,4(R1)            R0=N'FIELDS                                  
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                                                             
         MVC   TGBYTE2,0(R1)       STATUS BYTE                                  
         MVC   TGBYTE,11(R1)       ELEMENT TYPE FOR SOME                        
         SPACE 1                                                                
         MVC   ELCODE,3(R1)        SET ELEMENT CODE                             
CHRO1    XR    RF,RF                                                            
         CLI   ELCODE,TAFNELQ      FREE-FORM NAME ELEMENT                       
         BE    CHRO1B                                                           
         CLI   ELCODE,TANUELQ      OR FREE-FORM NUMBER ELEMENT                  
         BE    CHRO1B                                                           
         CLI   ELCODE,TACMELQ      OR COMMENT ELEMENT                           
         BNE   *+8                                                              
CHRO1B   LA    RF,1                NEED A SEARCH ARGUMENT                       
         SPACE 1                                                                
         GOTO1 GETLL,DMCB,((RF),TGBYTE)  GET THE ELEMENT                        
         BNE   CHRO3                                                            
         L     R3,TGELEM                                                        
CHRO1D   ZIC   RF,1(R3)                                                         
         AHI   RF,-3               RF=L'DATA-1                                  
         LA    R3,2(R3)            R3=A(DATA)                                   
         SPACE 1                                                                
         CLI   ELCODE,TANAELQ      ALL EXCEPT NAME                              
         BE    CHRO2                                                            
         CLI   ELCODE,TASNELQ      AND SHORT NAME                               
         BE    CHRO2                                                            
         CLI   ELCODE,TAA2ELQ      AND 2ND TYPE OF ADDRESS                      
         BE    CHRO2                                                            
         BCTR  RF,0                HAVE TYPE, SO SKIP IT                        
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
CHRO2    BAS   RE,CLEARFLD         CLEAR THE FIELD                              
         BAS   RE,FILLFLD          FILL FIELD                                   
         BE    CHRO5               NO MORE TO DISPLAY                           
         BCT   R0,CHRO2            LOOP FOR N'FIELDS                            
         B     CHROX                                                            
         SPACE 1                                                                
CHRO3    XR    R3,R3               DIDN'T FIND ELEMENT                          
         SPACE 1                                                                
         CLI   ELCODE,TANAELQ      IF LOOKING FOR LONG NAME                     
         BE    *+12                                                             
         CLI   ELCODE,TASNELQ      OR SHORT NAME                                
         BNE   CHRO4                                                            
         L     R1,AIO                                                           
         CLI   0(R1),TLW4CDQ       AND THIS IS W4 RECORD                        
         BNE   *+16                                                             
         BRAS  RE,BLDW4NME         BUILD DUMMY NAME ELEMENT IN WORK             
         LA    R3,WORK                                                          
         B     CHRO1D              AND GO BACK                                  
         SPACE 1                                                                
         CLI   ELCODE,TASNELQ      IF WE WERE LOOKING FOR SHORT NAME            
         BNE   CHRO4                                                            
         TM    TGBYTE2,X'80'       AND USER WILL TAKE ANY NAME                  
         BZ    CHRO4                                                            
         MVI   ELCODE,TANAELQ      THEN SET TO USE REGULAR NAME                 
         B     CHRO1                                                            
         SPACE 1                                                                
CHRO4    BAS   RE,CLEARFLD         CLEAR REMAINING FIELDS                       
CHRO5    LTR   R4,R4                                                            
         BZ    CHROX                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         CLI   0(R4),0                                                          
         BE    CHROX                                                            
         TM    1(R4),X'20'         BUMP TO NEXT UNPROTECTED                     
         BO    CHRO5                                                            
         BCT   R0,CHRO4            LOOP FOR N'FIELDS                            
         SPACE 1                                                                
CHROX    LTR   R3,R3               RETURN CC NOT EQUAL IF EL. NOT FOUND         
         BZ    NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO ADD A NAME ELEMENT                                    
         SPACE 1                                                                
*                                  P1 BYTE 0    = N'FIELDS                      
*                                     BYTE 3    = TANAELQ=NAME                  
*                                                 TASNELQ=SHORT NAME            
*                                                 TAFNELQ=FREEFORM NAME         
*                                                 TANUELQ=FREEFORM NUM          
*                                                 TACMELQ=COMMENT               
*                                  P2 BYTE  0     X'80'=NAME OPTIONAL           
*                                                 X'40'=CASE SENSITIVE          
*                                                 X'20'=1ST CHAR MUST           
*                                                       BE NON-SPACE            
*                                     BYTES 1-3 = A(FIELD)                      
*                                  P3 BYTE 3    = TYPE FOR TAFNELQ,             
*                                                      TANUELQ, TACMELQ         
         SPACE 1                                                                
NAMIN    DS    0H                                                               
         L     R2,4(R1)            R2=A(FIELD)                                  
         ZIC   R0,0(R1)            R0=N'FIELDS                                  
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                                                             
         MVC   TGBYTE2,4(R1)       STATUS                                       
         MVC   TGBYTE,11(R1)       ELEMENT TYPE                                 
         MVC   ELCODE,3(R1)        ELCODE                                       
         SPACE 1                                                                
         XR    RF,RF               SET TO DELETE CURRENT                        
         CLI   ELCODE,TAFNELQ      FREE-FORM NAME ELEMENT                       
         BE    NAMI0                                                            
         CLI   ELCODE,TANUELQ      OR FREE-FORM NUMBER ELEMENT                  
         BE    NAMI0                                                            
         CLI   ELCODE,TACMELQ      OR COMMENT ELEMENT                           
         BNE   *+8                                                              
NAMI0    LA    RF,1                NEED A SEARCH ARGUMENT                       
         GOTO1 DELLL,DMCB,((RF),TGBYTE)                                         
         SPACE 1                                                                
         CLI   5(R2),0             IS A NAME PRESENT                            
         BNE   *+16                                                             
         TM    TGBYTE2,X'80'       NO - IS THAT OK                              
         BZ    FLDMISS             NO                                           
         B     NO                  YES, BUT RETURN CC NE                        
         SPACE 1                                                                
         CLI   8(R2),C' '          IF FIRST CHARACTER A SPACE                   
         BNE   *+12                                                             
         TM    TGBYTE2,X'20'       RETURN ERROR IF THAT IS NOT                  
         BO    FLDINV              ALLOWED                                      
         SPACE 1                                                                
         MVI   ELEMENT,C' '        BUILD NEW ELEMENT                            
         MVC   ELEMENT+1(L'ELEMENT-1),ELEMENT                                   
         SPACE 1                                                                
         MVC   ELEMENT(1),ELCODE   ELEMENT CODE                                 
         SPACE 1                                                                
         LA    R3,ELEMENT+2        R3=A(DATA IN EL.)                            
         CLI   ELEMENT,TAFNELQ     FREE-FORM NAME ELEMENT                       
         BE    NAMI1                                                            
         CLI   ELEMENT,TANUELQ     OR FREE-FORM NUMBER ELEMENT                  
         BE    NAMI1                                                            
         CLI   ELEMENT,TACMELQ     OR COMMENT ELEMENT                           
         BNE   NAMI2                                                            
NAMI1    MVC   ELEMENT+2(1),TGBYTE MOVE TYPE TO ELEMENT                         
         LA    R3,1(R3)            BUMP PAST IT                                 
         SPACE 1                                                                
NAMI2    ZIC   R4,5(R2)            R4=L'DATA IN FIELD                           
         LTR   R4,R4               TEST FOR INPUT                               
         BZ    NAMI6                                                            
         CHI   R0,1                IF NOT LAST LINE                             
         BE    NAMI4                                                            
         ZIC   R4,0(R2)            USE L'FIELD                                  
         AHI   R4,-16              LESS L'HEADERS                               
         SPACE 1                                                                
NAMI4    BCTR  R4,0                LESS ONE FOR EXECUTE                         
         EX    R4,MVCDATA          MOVE DATA TO EL.                             
         TM    TGBYTE2,X'40'       CASE SENSITIVE?                              
         BO    *+8                 YES, SKIP THE 'OR' WITH SPACES               
         EX    R4,OCSPACES         'OR' IN SPACES                               
         LA    R3,1(R4,R3)         BUMP TO NEXT SPOT IN EL.                     
NAMI6    BAS   RE,BUMP             AND TO NEXT FIELD                            
         BCT   R0,NAMI2                                                         
         SPACE 1                                                                
         BCTR  R3,0                POINT TO LAST CHARACTER                      
         CLI   0(R3),C' '          SHUFFLE BACK TO LAST NON-BLANK               
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         SPACE 1                                                                
         LA    RF,ELEMENT-1        DERIVE L'ELEMENT                             
         SR    R3,RF                                                            
         STC   R3,ELEMENT+1                                                     
         SPACE 1                                                                
         CLI   ELEMENT,TASNELQ     SHORT NAME ELEMENT                           
         BNE   *+8                                                              
         MVI   ELEMENT+1,TASNLNQ   HAS FIXED LENGTH                             
         SPACE 1                                                                
         MVI   TGBYTE,3                                                         
         CLI   ELCODE,TANAELQ      ENSURE ELEMENT MEETS MINIMUM                 
         BE    *+8                 LENGTH                                       
         MVI   TGBYTE,4                                                         
         CLC   ELEMENT+1(1),TGBYTE                                              
         BL    NO                                                               
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         B     YES                                                              
         SPACE 3                                                                
MVCDATA  MVC   0(0,R3),8(R2)       ** EXECUTED - MOVE DATA TO EL. **            
OCSPACES OC    0(0,R3),SPACES      ** EXECUTED - OR SPACES TO EL. **            
         EJECT                                                                  
*              ROUTINE TO ADD AN ADDRESS ELEMENT                                
         SPACE 1                                                                
*                                  P1 BYTES 0   X'80'=ADDR OPTIONAL             
*                                               LOBS=N'FIELDS IN ADDR           
*                                     BYTES 1-3 = A(1ST FIELD HEADER)           
ADDRIN   DS    0H                                                               
         L     R2,0(R1)                                                         
         MVC   TGBYTE,0(R1)                                                     
         MVI   ELCODE,TAADELQ                                                   
         GOTO1 REMELEM             FIRST DELETE EXISTING ELEMENT                
         SPACE 1                                                                
         LA    R4,ELEMENT                                                       
         USING TAADD,R4                                                         
         MVI   TAADEL,TAADELQ      BUILD THE ELEMENT                            
         LA    R3,TAADADD                                                       
         XR    R1,R1               R1=N'LINES USED                              
         XR    RF,RF                                                            
         ZIC   R0,TGBYTE           R0=MAX N'LINES                               
         SLL   R0,25                                                            
         SRL   R0,25                                                            
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,4                                                             
         LR    R5,R2               R5=A(1ST LINE)                               
         SPACE 1                                                                
ADDRIN1  CLI   5(R5),0             IF THERE'S INPUT IN THIS FIELD               
         BE    ADDRIN2                                                          
         MVC   0(L'TAADADD,R3),SPACES PRE-CLEAR FIELD IN ELEMENT                
         IC    RF,5(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R5)       MOVE DATA TO ELEMENT                         
         LA    R3,L'TAADADD(R3)    BUMP TO NEXT FIELD IN ELEMENT                
         LA    R1,1(R1)            ADD 1 TO N'LINES                             
         SPACE 1                                                                
ADDRIN2  IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         TM    1(R5),X'20'         BUMP TO NEXT UNPROTECTED FIELD               
         BO    ADDRIN2                                                          
         BCT   R0,ADDRIN1                                                       
         SPACE 1                                                                
         LTR   R1,R1               ANY LINES I/P                                
         BNZ   *+16                                                             
         TM    TGBYTE,X'80'        NO - IS FIELD OPTIONAL                       
         BZ    FLDMISS             NO                                           
         B     NO                  YES, BUT RETURN CC NE                        
         SPACE 1                                                                
         STC   R1,TAADLNES         SET N'LINES IN ELEMENT                       
         MH    R1,=AL2(L'TAADADD)                                               
         LA    R1,TAADADD-TAADD(R1)                                             
         STC   R1,TAADLEN          SET ELEMENT LENGTH                           
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY LAST CHANGE INFORMATION                       
         SPACE 1                                                                
*                                  P1, BYTES 1-3=A(FIELD HEADER)                
*                                      BYTE 0  X'80'=FILTER ON TWASCR           
*                                              X'40'=P1 IS A(O/P AREA)          
*                                              X'20'=RTRN A(EL) IN P1           
ACTVOUT  DS    0H                                                               
         L     R4,0(R1)            LOAD PARAMETER                               
         MVC   TGBYTE,0(R1)                                                     
         XC    0(4,R1),0(R1)       CLEAR AREA FOR RTRN OF A(EL)                 
         SPACE 1                                                                
         TM    TGBYTE,X'20'        IF A(EL) TO BE RETURNED                      
         BO    ACTVO1                                                           
         TM    TGBYTE,X'40'        IF POINTING TO FLDHDR                        
         BO    *+16                                                             
         BAS   RE,CLEARFLD         GO CLEAR SCREEN FIELD                        
         NI    1(R4),X'F3'         DEFAULT TO NORMAL INTENSITY                  
         B     ACTVO1                                                           
         SPACE 1                                                                
         XC    0(17,R4),0(R4)      ELSE CLEAR IT HERE                           
         AHI   R4,-8               AND BUMP BACK L'HDR FOR OUTPUT               
         SPACE 1                                                                
ACTVO1   MVI   ELCODE,TAACELQ      SET ELEMENT CODE                             
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ACTVO2   BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAACD,R3                                                         
         TM    TGBYTE,X'80'        IF NOT FILTERING ON SCREEN                   
         BO    *+16                                                             
         CLI   TAACSCR,0           IGNORE ELS WITH SCREEN IN THEM               
         BNE   ACTVO2                                                           
         B     *+14                                                             
         CLC   TAACSCR,TWASCR      ELSE TEST SCREEN MATCHES                     
         BNE   ACTVO2                                                           
         TM    TGBYTE,X'20'        IF A(EL) TO BE RETURNED                      
         BNO   *+12                                                             
         ST    R3,0(R1)            0(R1) = A(TAACD EL)                          
         B     XIT                                                              
         MVC   8(8,R4),TAACSTAF    DISPLAY STAFF NAME                           
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,17(R4)) AND DATE                     
         SPACE 1                                                                
         TM    TGBYTE,X'40'        IF POINTING TO FLDHDR                        
         BO    ACTVOX                                                           
         CLC   TAACCDTE,TGTODAY1   AND LAST CHANGED DATE IS TODAY               
         BNE   ACTVOX                                                           
         OI    1(R4),X'08'         SET TO HIGH INTENSITY                        
ACTVOX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD ACTIVITY ELEMENT TO RECORD                        
         SPACE 1                                                                
*                                  P1  BYTE 0  X'80'=FILTER ON TWASCR           
*                                              X'40'=DON'T DO ADDELEM           
ACTVINL  NTR1                                                                   
ACTVIN   DS    0H                                                               
         MVC   TGBYTE,0(R1)                                                     
         MVI   ELCODE,TAACELQ      SET ELEMENT CODE                             
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ACTVI2   BAS   RE,NEXTEL                                                        
         BNE   ACTVI4                                                           
         TM    TGBYTE,X'80'        IF NOT FILTERING ON SCREEN                   
         BO    *+16                                                             
         CLI   TAACSCR,0           IGNORE ELS WITH SCREEN IN THEM               
         BNE   ACTVI2                                                           
         B     *+14                                                             
         CLC   TAACSCR,TWASCR      ELSE TEST SCREEN MATCHES                     
         BNE   ACTVI2                                                           
         MVI   TAACEL,X'FF'        DELETE THIS ELEMENT                          
         GOTO1 HELLO,DMCB,(C'D',=CL8'TALFIL'),(X'FF',AIO),0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE                                                                  
         DROP  R3                                                               
         SPACE 1                                                                
ACTVI4   LA    R4,ELEMENT                                                       
         USING TAACD,R4                                                         
         XC    TAACEL(TAACLNQ),TAACEL                                           
         MVI   TAACEL,TAACELQ      BUILD THE ELEMENT                            
         MVI   TAACLEN,TAACLNQ                                                  
         MVC   TAACID,TWAORIG      USER ID NUMBER                               
         MVC   TAACSTAF,TGCTSTAF   STAFF ID                                     
         MVC   TAACCDTE,TGTODAY1   TODAY'S DATE                                 
         TIME  DEC                                                              
         STCM  R0,14,TAACCTIM      CURRENT TIME                                 
         TM    TGBYTE,X'80'        IF CALL REQUESTS IT                          
         BZ    *+10                                                             
         MVC   TAACSCR,TWASCR      SET SCREEN CODE                              
         SPACE 1                                                                
         TM    TGBYTE,X'40'        UNLESS SPEC REQUESTED OTHERWISE              
         BO    XIT                                                              
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ROUTINE TO CLEAR A FIELD                                   
         SPACE 1                                                                
*                                  R4=A(FIELD HEADER)                           
CLEARFLD DS    0H                                                               
         MVC   TGNAME,SPACES       CLEAR GENERAL AREA                           
CLEARFL2 LTR   R4,R4                                                            
         BZR   RE                                                               
         ZIC   R1,0(R4)                                                         
         AHI   R1,-9               R1=L'FIELD-1                                 
         TM    1(R4),X'02'                                                      
         BZ    *+8                                                              
         AHI   R1,-8               HANDLE EXTENDED HEADERS                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R4),8(R4)       CLEAR THE FIELD                              
         MVI   5(R4),0                                                          
         OI    6(R4),X'80'                                                      
         BR    RE                  RETURN R1                                    
         EJECT                                                                  
*              LOCAL ROUTINE TO FILL A FIELD                                    
         SPACE 1                                                                
*                                  R4=A(FIELD HEADER)                           
*                                  R1=L'FIELD-1                                 
*                                  R3=A(DATA)                                   
*                                  RF=L'DATA-1                                  
FILLFLD  DS    0H                                                               
         ST    RE,TGFULL           SAVE RE                                      
         LA    RE,L'TGNAME-1       SET L'GENERAL NAME FIELD                     
         CR    RE,RF               COMPARE TO L'DATA                            
         BNH   *+6                                                              
         LR    RE,RF               FIELD IS BIGGER - MOVE FOR L'DATA            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TGNAME(0),0(R3)     MOVE DATA TO GLOBAL STORAGE                  
         SPACE 1                                                                
         LTR   R4,R4               TEST WE HAVE A(FIELD)                        
         BZ    FILLX               NO - RETURN CC EQ (DONE)                     
         CR    R1,RF               COMPARE L'FIELD TO L'DATA                    
         BNH   *+6                                                              
         LR    R1,RF               FIELD IS BIGGER - MOVE FOR L'DATA            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),0(R3)       MOVE DATA TO SCREEN                          
         LA    RE,1(R1)                                                         
         STC   RE,5(R4)            SET L'DATA IN FIELD HEADER                   
         SR    RF,R1                                                            
         BZ    FILLX               NO MORE TO DISPLAY - RETURN CC EQUAL         
         SPACE 1                                                                
         BCTR  RF,0                RF=L'DATA REMAINING TO BE DISPLAYED          
         LA    R3,1(R1,R3)         BUMP TO NEXT DATA                            
         XR    R1,R1                                                            
         IC    R1,0(R4)            AND TO NEXT FIELD                            
         AR    R4,R1                                                            
         TM    1(R4),X'20'         BUMP TO NEXT UNPROTECTED                     
         BO    *-10                                                             
         LTR   R4,R4               MORE TO COME - RETURN CC NOT ZERO            
         SPACE 1                                                                
FILLX    L     RE,TGFULL           RESET RE                                     
         BR    RE                  RETURN CC                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE A USER ID                                    
         SPACE 1                                                                
*                                  P1=A(FIELD) OR W/S                           
*                                  P1 BYTE 0  X'80'=NON-SCREEN DATA             
*                                             X'40'=P2 HAS A(NAME)              
*                                             X'20'=RETURN ERRORS               
*                                             X'10'=USERID > 6 CHARS            
*                                  P2=A(NAME FIELD)-SEE X'40' BIT ABOVE         
USERVAL  L     R2,0(R1)                                                         
         MVC   TGBYTE,0(R1)                                                     
         MVC   TGFULL,4(R1)                                                     
         TM    0(R1),X'40'                                                      
         BO    *+10                                                             
         XC    TGFULL,TGFULL                                                    
         MVC   WORK,0(R2)          ASSUME NOT FROM SCREEN                       
         TM    TGBYTE,X'80'                                                     
         BO    USERV2                                                           
         GOTO1 ANY                 IT IS FROM SCREEN - REQUIRE IT               
         SPACE 1                                                                
USERV2   MVC   FILENAME,=CL8'CTFILE' SET TO READ CONTROL FILE                   
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTIKEY,R3                                                        
         MVI   CTIKTYP,C'I'        BUILD KEY FOR ID RECORD                      
         MVC   CTIKID,WORK                                                      
         CLI   CTIKID,0            IF NOT ID#                                   
         BE    USERV2A                                                          
         MVC   CTIKID+8(2),SPACES  MOVE IN RIGHT ALIGNED SPACES                 
         OC    CTIKID+6(2),SPACES                                               
         TM    TGBYTE,X'10'                                                     
         BO    USERV2A                                                          
         MVC   CTIKID+6(2),SPACES                                               
USERV2A  GOTO1 HIGH                READ ID RECORD ON CONTROL FILE               
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,C'N'                                                       
         CLC   CTIKEY,KEYSAVE      DID WE GET THE RECORD                        
         BE    USERV3                                                           
         TM    TGBYTE,X'80'        NO - GIVE ERROR IF HAVE A(FLD)               
         BZ    RECNTFND                                                         
         TM    TGBYTE,X'20'        OR RETURN CC NE IF RETURNING ERRORS          
         BO    NO                                                               
         DC    H'0'                ELSE DIE - USERID VANISHED                   
         SPACE 1                                                                
USERV3   L     R3,AIO                                                           
         LA    R3,CTIDATA                                                       
         MVI   ELCODE,X'02'        NOW LOOK FOR DESCRIPTION ELEMENT             
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSCD,R3                                                        
         CLI   CTDSCLEN,4          IF ELEMENT IS 4 BYTES LONG                   
         BNE   USERV4                                                           
         MVC   TGUSER,CTDSC        THEN GET USER ID NUMBER FROM ELEMENT         
         MVC   TGUSERID,WORK       AND EBCDIC USER ID FROM KEY                  
         MVC   TGUSERI2,WORK                                                    
         B     USERV5                                                           
         SPACE 1                                                                
USERV4   MVC   TGUSER,WORK+8       ELSE GET USER ID NUMBER FROM KEY             
         MVC   TGUSERID,CTDSC      AND EBCDIC USER ID FROM ELEMENT              
         MVC   TGUSERI2,SPACES                                                  
         MVC   TGUSERI2,CTDSC                                                   
         SPACE 1                                                                
USERV5   MVI   ELCODE,X'06'        GET ALPHA ID ELEMENT                         
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTAGYD,R3                                                        
         MVC   TGALPH,CTAGYID      SAVE 2-CHAR. AGENCY ALPHA ID                 
         SPACE 1                                                                
         XC    TGACCINF,TGACCINF                                                
         MVI   ELCODE,X'21'        GET SYSTEM ELEMENT FOR ACC                   
USERV6   BAS   RE,NEXTEL                                                        
         BNE   USERV8                                                           
         USING CTSYSD,R3                                                        
         CLI   CTSYSNUM,6          NEED SYSTEM ACC                              
         BNE   USERV6                                                           
         MVC   TGACCSE,CTSYSSE     SAVE ACCPAK SE NUMBER                        
         MVC   TGACCHX,CTSYSAGB    AND HEXCOMP                                  
         SPACE 1                                                                
USERV8   L     R3,AIO                                                           
         USING CTIKEY,R3                                                        
         LA    R3,CTIDATA                                                       
         L     R4,TGFULL           R4=A(NAME FIELD)                             
         BAS   RE,CLEARFLD         CLEAR IT                                     
         MVI   ELCODE,X'30'        LOOK FOR DEST EL.                            
         BAS   RE,NEXTEL                                                        
         BNE   USERVX                                                           
         USING CTDSTD,R3                                                        
         LA    R3,CTDSTNAM         R3=A(NAME)                                   
         LA    RF,L'CTDSTNAM-1     RF=L'NAME-1                                  
         BAS   RE,FILLFLD          MOVE IN NAME                                 
         SPACE 1                                                                
USERVX   B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD KEYS AND VALIDATE RECORDS                       
         SPACE 1                                                                
*                                  P1=RECORD EQUATE                             
*                                  P1 BYTE 0  X'80'=SKIP LIMCHK                 
*                                             X'40'=CHECK FOR FLIST             
*                                             X'20'=VERSIONS OK (TLCOI)         
*                                             X'10'=DO CLI GROUP ACCESS         
*                                  P2=A(FIELD) OR W/S (OR BIN. ZEROS)           
*                                  P2 BYTE 0  X'80'=NON-SCREEN DATA             
*                                             X'40'=BUILD KEY ONLY              
*                                             X'20'=DO GETREC                   
*                                             X'10'=READ FOR UPDATE             
*                                             X'08'=P3 HAS A(NAME)              
*                                             X'04'=RETURN ERRORS               
*                                             X'02'=DON'T SET VAL. BIT          
*                                             X'01'=ALLOW AGY '999999'          
*                                  P3=A(NAME FIELD)-SEE X'08' BIT ABOVE         
*                                  P3 BYTE 0  SUB-RECORD EQUATE (X'24')         
RECVALL  NTR1                                                                   
RECVAL   DS    0H                                                               
         MVC   KEYSAVE(1),3(R1)    SAVE RECORD CODE IN KEYSAVE                  
         MVC   TGBYTE3,0(R1)        ''  PARAMETER BITS                          
         MVC   TGBYTE,4(R1)                                                     
         MVC   TGFULL,8(R1)         ''  A(NAME FIELD) IF AROUND                 
         MVC   TGRVSUB,8(R1)        ''  RECVAL SUB REC NUMBER                   
         MVI   TGBYTE4,0           INITIALIZE LOOP CONTROLLER                   
         XR    R2,R2                                                            
         ICM   R2,7,5(R1)          R2=A(FIELD HEADER OR W/S) OR ZEROS           
         BNZ   *+8                                                              
         OI    TGBYTE,X'80'        INSURE NON-SCREEN DATA SET                   
         SPACE 1                                                                
         TM    TGBYTE,X'80'        IF DATA NOT FROM SCREEN                      
         BZ    *+8                                                              
         OI    TGBYTE,X'04'        THEN ALWAYS RETURN ERRORS                    
         SPACE 1                                                                
         TM    TGBYTE,X'08'        IF USER WANTS NAME                           
         BZ    *+8                                                              
         OI    TGBYTE,X'20'        INSURE WE GET THE RECORD                     
         SPACE 1                                                                
         TM    TGBYTE3,X'40'       IF CHECKING FOR FLIST                        
         BZ    *+12                                                             
         BAS   RE,FLIST            LOOK FOR IT                                  
         BE    RVALX               DONE IF FLIST                                
                                                                                
         CLI   KEYSAVE,TLTMCDQ     IF READING TIMESHEETS                        
         BNE   RVAL3                                                            
         MVI   TGBYTE2,0                                                        
         TM    TGFASTAT,TGFROMFA+TGRDWBTS                                       
         BZ    RVAL3               SET TO READ MF OR DDLINK                     
         MVI   TGBYTE2,TLTMSWEB    TIMESHEETS                                   
         NI    TGFASTAT,X'FF'-TGRDWBTS                                          
                                                                                
RVAL3    LA    R5,RECTAB           R5=A(RECORD TABLE)                           
         CLI   KEYSAVE,TLPMCDQ     X'24' = SPECIAL RECORDS                      
         BNE   RVAL4                                                            
         L     R5,=A(RECTAB2)                                                   
         A     R5,TGRELOV                                                       
         MVC   KEYSAVE+1(1),TGRVSUB                                             
         USING RECD,R5                                                          
RVAL4    CLI   0(R5),X'FF'                                                      
         BE    RVALERR1                                                         
         CLI   KEYSAVE,TLPMCDQ     X'24' = SPECIAL RECORDS                      
         BE    RVAL4S              YES, MATCH ON SUB RECORD CODE                
         CLC   RECCD,KEYSAVE       NO, MATCH ON RECORD CODE                     
         BE    RVAL6                                                            
         B     RVAL5                                                            
                                                                                
RVAL4S   CLC   RECCD,TGRVSUB       MATCH ON SUB RECORD CODE                     
         BE    RVAL6                                                            
                                                                                
RVAL5    ZIC   RE,RECLEN           BUMP TO NEXT RECORD IN TABLE                 
         AR    R5,RE                                                            
         B     RVAL4                                                            
         SPACE 1                                                                
RVAL6    CLI   KEYSAVE,TLCMCDQ     COMMENT RECORD?                              
         BNE   RVAL7                                                            
         GOTO1 =A(GETCOM),RR=TGRELOV   GET CORRECT COMMENT TYPE                 
RVAL7    XC    KEY,KEY             INITIALIZE KEY                               
         MVC   KEY(1),KEYSAVE                                                   
         CLI   KEYSAVE,TLPMCDQ     X'24' = SPECIAL RECORDS                      
         BNE   *+10                                                             
         MVC   KEY(2),KEYSAVE                                                   
                                                                                
         ZIC   R0,RECNFLDS         R0=N'FIELDS IN TABLE                         
         LTR   R0,R0                                                            
         BZ    RVAL12                                                           
         LA    RF,RECFLDS          RF=A(FIRST FIELD ENTRY)                      
         USING FLDD,RF                                                          
RVAL8    ZIC   R3,FLDKDSP                                                       
         LA    R3,KEY(R3)          R3=A(FIELD IN KEY)                           
         XR    R4,R4                                                            
         ICM   R4,3,FLDGDSP                                                     
         LA    R4,TGD(R4)          R4=A(FIELD IN GLOBAL STORAGE)                
         ZIC   R1,FLDLEN           R1=L'FIELD-1                                 
         SPACE 1                                                                
         CHI   R0,1                SKIP IF THIS IS NOT THE LAST FIELD           
         BNE   RVAL10                                                           
         LTR   R2,R2               TEST IF USER PASSED A(FIELD)                 
         BZ    RVAL10                                                           
         MVC   WORK,0(R2)          ASSUME DATA IS NOT ON SCREEN                 
         SPACE 1                                                                
         TM    TGBYTE,X'80'        SKIP IF DATA NOT ON SCREEN                   
         BO    RVAL9                                                            
         MVC   WORK,8(R2)          INSURE WE HAVE PADDED FIELD IN WORK          
         CLI   KEYSAVE,TLMTCDQ                                                  
         BE    RVAL8A                                                           
         CLI   KEYSAVE,TLMTALDQ                                                 
         BE    RVAL8A                                                           
         OC    WORK,SPACES                                                      
RVAL8A   TM    4(R2),X'20'         IF FIELD NOT PREVIOUSLY VALIDATED            
         BZ    *+12                                                             
         EX    R1,CLCTOGLB         OR IF IT DOESN'T MATCH GLOBAL STOR.          
         BE    *+8                                                              
         BAS   RE,INVALSCR         INVALIDATE REMAINING KEY FIELDS              
         SPACE 1                                                                
         CLI   5(R2),0             TEST NOTHING IN FIELD                        
         BNE   RVAL9                                                            
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   *+12                                                             
         TM    TGBYTE,X'40'        & BUILDING KEY ONLY THEN REQUIRE IT          
         BO    FLDMISS                                                          
         EX    R1,CLCGLBSP         CHECK FOR SOMETHING IN GLOBAL STOR.          
         BNH   RVALERR2                                                         
         EX    R1,MVCTOSCR         MOVE TO SCREEN FROM GLOBAL STORAGE           
         LA    RE,1(R1)                                                         
         STC   RE,5(R2)            SET L'FIELD                                  
         OI    6(R2),X'80'         AND TRANSMIT                                 
         LA    RE,TGDUC            IF HANDLING GLOBAL DUE COMP CODE             
         CR    RE,R4                                                            
         BNE   RVAL10                                                           
*        BAS   RE,DISPDUC          MAKE SURE IT IS DISPLAYABLE                  
         GOTO1 =A(DISPDUC),RR=TGRELOV MAKE SURE IT IS DISPLAYABLE               
         B     RVAL10                                                           
         SPACE 1                                                                
RVAL9    EX    R1,MVCTOGLB         MOVE TO GLOBAL STORAGE FROM WORK             
         SPACE 1                                                                
RVAL10   EX    R1,MVCTOKEY         MOVE TO KEY FROM GLOBAL STORAGE              
         LA    RE,TGEPI            IF HANDLING GLOBAL EPISODE NUMBER            
         CR    RE,R4                                                            
         BNE   *+8                                                              
         EX    R1,CMPLMKEY         NEED TO COMPLEMENT FIELD IN KEY              
         CLI   KEY,TLCAGCDQ        IF READING CAST GUARS                        
         BNE   RVAL11                                                           
         LA    RE,TGGUA            AND IF HANDLING GLOBAL GUAR. NUMBER          
         CR    RE,R4                                                            
         BNE   *+8                                                              
         EX    R1,CMPLMKEY         NEED TO UN-COMMPLEMENT FIELD IN KEY          
RVAL11   CLI   KEY,TLJBCDQ         IF READING JOB RECORDS                       
         BNE   RVAL11X                                                          
         LA    RE,TGDATE           AND IF HANDLING GLOBAL DATE                  
         CR    RE,R4                                                            
         BNE   *+8                                                              
         EX    R1,CMPLMKEY         NEED TO COMMPLEMENT FIELD IN KEY             
         SPACE 1                                                                
RVAL11X  LA    RF,FLDNEXT          PROCESS NEXT FIELD                           
         BCT   R0,RVAL8                                                         
         SPACE 1                                                                
RVAL12   TM    TGBYTE3,X'80'       TEST WHETHER TO SKIP CHECK                   
         BO    *+12                                                             
         BRAS  RE,LIMCHK           CHECK FOR LIMIT ACCESS RESTRICTIONS          
         BNE   RVALERR1                                                         
         TM    TGBYTE,X'40'        USER WANTS ONLY TO BUILD THE KEY             
         BZ    RVAL14                                                           
         OC    KEY+1(L'TLDRKEY-1),KEY+1 IF WE DON'T HAVE ANYTHING               
         BZ    RVAL5                    LOOK FOR ANOTHER ENTRY IN TABLE         
         B     RVALX                                                            
         SPACE 1                                                                
RVAL14   TM    TGBYTE,X'80'+X'20'  IF DATA ON SCREEN & REC. NOT REQ'D           
         BNZ   *+12                                                             
         TM    4(R2),X'20'         IF FIELD HASN'T CHANGED                      
         BO    RVALX               DON'T BOTHER RE-READING                      
         SPACE 1                                                                
         TM    TGBYTE,X'10'        USER WANTS READ FOR UPDATE                   
         BZ    *+8                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                GET IT                                       
         SPACE 1                                                                
RVAL14B  BAS   RE,SETLCMP          SETS RF=(L'KEY) FOR COMPARE                  
         EX    RF,KEYCOMP                                                       
         BE    RVAL14C                                                          
         SPACE 1                                                                
         CLI   KEYSAVE,TLCOICDQ    IF READING COMMERCIAL ID KEY                 
         BNE   RVAL5                                                            
         TM    TGBYTE3,X'20'       AND VERSIONS ARE OK TO FIND                  
         BZ    RVAL5                                                            
         TM    TGBYTE4,X'80'       AND WE'VE ALREADY LOOPED ONCE                
         BO    RVAL5               LOOKING FOR VERSION 0 (AKA 1)                
         OI    TGBYTE4,X'80'       NOW LETS LOOP AGAIN AND LOOK                 
         B     RVAL4               FOR HIGHER VERSIONS                          
         SPACE 1                                                                
RVAL14C  CLI   KEY,TLCOICDQ        FOR COMMERCIAL ID KEY                        
         BNE   RVAL14N                                                          
         TM    KEY+TLDRSTAT-TLDRD,X'40'  DON'T RETURN IF COPIED                 
         BO    RVAL14S                                                          
         CLI   KEY+TLCOIVER-TLCOPD,0     IF VERSION 0 (AKA 1) KEY               
         BE    RVAL15                    FOUND, ALWAYS RETURN IT                
         TM    TGBYTE4,X'80'             IF LOOKING FOR VERSIONS 2              
         BZ    RVAL14S                   OR HIGHER                              
         B     RVAL15                    AND WE FIND IT, RETURN IT              
RVAL14N  CLI   KEY,TLNXCDQ         FOR NETWORK TRANSFER KEY                     
         BNE   RVAL14Q                                                          
         TM    KEY+TLDRSTAT-TLDRD,TLNXSDEL  DON'T RETURN IF DELETED             
         BZ    RVAL15                                                           
         B     RVAL14S                                                          
RVAL14Q  CLI   KEY,TLINCDQ         FOR INVOICE KEYS                             
         BL    RVAL15                                                           
         CLI   KEY,TLINPCDQ        DON'T RETURN IF DELETED                      
         BH    RVAL15                                                           
         TM    TGBYTE3,X'08'       IF WANT ALL INVOICES, BOTH UNDELETED         
         BNZ   RVAL15              AND SOFT DELETED ARE OK                      
         TM    TGBYTE3,X'04'       IF WANT ONLY UNDELETED INVOICES,             
         BNZ   RVAL14R             DO NOT RETURN SOFT DELETES                   
         TM    KEY+TLDRSTAT-TLDRD,TLINSDEL                                      
         BZ    RVAL15                                                           
         B     RVALERR1                                                         
RVAL14R  TM    KEY+TLDRSTAT-TLDRD,TLINSDEL  IF WANT ONLY DELETED INVS,          
         BZ    RVALERR1                     RETURN ONLY SOFT DELETES            
         B     RVAL15                                                           
RVAL14S  GOTO1 SEQ                 CHECK IF NEXT RECORD HAS SAME ISCII          
         B     RVAL14B                                                          
         SPACE 1                                                                
RVAL15   TM    TGBYTE,X'82'        UNLESS NOT A FIELD OR REQ. OTHERWISE         
         BNZ   *+8                                                              
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         SPACE 1                                                                
         CLI   KEY,TLMTCDQ         IF PRIMARY CNET/CSYS/MARKET KEY              
         BNE   *+10                                                             
         MVC   TGMTINTC,KEY+TLMTINUM-TLMTD         SAVE INTERNAL #              
         SPACE 1                                                                
         CLI   KEY,TLMTALDQ        IF TMKT ALPHA CODE KEY                       
         BNE   *+10                                                             
         MVC   TGMTINTC,KEY+TLMTALIN-TLMTPD        SAVE INTERNAL #              
         SPACE 1                                                                
         CLI   KEY,TLMTICDQ        IF CNET/CSYS/MARKET INT.CODE KEY             
         BNE   *+10                                                             
         MVC   TGMTCODE,KEY+TLMTICCD-TLMTPD               SAVE CODE             
         SPACE 1                                                                
         TM    TGBYTE,X'20'        DOES USER WANT THE RECORD                    
         BZ    RVALX                                                            
         TM    TGBYTE,X'10'        USER WANTS READ FOR UPDATE                   
         BZ    *+8                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         L     R3,AIO                                                           
         CLI   0(R3),TLCOCDQ       IF THIS IS COMMERCIAL                        
         BNE   RVAL16                                                           
         CLI   KEY,TLCOICDQ        AND READ WAS VIA COMML ID                    
         BNE   RVAL15A                                                          
         USING TLCOD,R3                                                         
         GOTOR LIMCHK2,DMCB,TLCOCLI CHECK LIMIT ACCESS                          
         BNE   RVALERR1                                                         
         SPACE 1                                                                
RVAL15A  MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   RVAL16                                                           
         USING TACOD,R3                                                         
         GOTO1 CTYPVALL,DMCB,TACOTYPE  MOVE COMML TYPE TO GLOBAL                
         GOTO1 MEDVALL,DMCB,TACOMED    MOVE MEDIA TO GLOBAL                     
         TM    TGBYTE3,X'10'       IF CHECKING CLI GROUP ACCESS                 
         BZ    RVAL15X                                                          
         OC    TGCLGACC,TGCLGACC   AND IT IS DEFINED                            
         BZ    RVAL15X                                                          
         CLC   TGCLGACC,TACOCLG    CHECK IT AGAINST RECORD REQUESTED            
         BNE   RVALERR3                                                         
RVAL15X  MVC   TGCLG,TACOCLG           MOVE CLIENT GROUP TO GLOBAL              
         SPACE 1                                                                
RVAL16   L     R3,AIO              RESTORE R3 TO AIO                            
         CLI   0(R3),TLINCDQ       IF THIS IS AN INVOICE RECORD                 
         BNE   RVAL800                                                          
         CLI   KEY,TLINCDQ         AND READ WAS VIA ACTIVE POINTER              
         BNE   RVAL800                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   RVAL800                                                          
         USING TAPDD,R3            AND WAS PAID                                 
         GOTOR LIMCHK2,DMCB,TAPDCLI CHECK LIMIT ACCESS                          
         BNE   RVALERR1                                                         
         SPACE 1                                                                
RVAL800  L     R3,AIO              RESTORE R3 TO AIO                            
         CLI   0(R3),TLAYCDQ       IF AGENCY RECORD READ, SAVE STAT7            
         BNE   RVAL900                                                          
         USING TAAYD,R3                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   RVAL900                                                          
         MVI   TGAYSTA7,0                                                       
         CLI   TAAYLEN,TAAYLNQ                                                  
         BL    RVAL900                                                          
         MVC   TGAYSTA7,TAAYSTA7                                                
                                                                                
RVAL900  TM    TGBYTE,X'08'        DOES USER WANT NAME                          
         BZ    RVALX                                                            
         L     R4,TGFULL           RESTORE A(NAME FIELD)                        
         LA    R3,TASNELQ          ASSUME WE WANT SHORT NAME                    
         TM    1(R4),X'02'         IF THERE IS AN EXTENDED HEADER               
         BNO   RVAL930                                                          
         CLI   0(R4),16+L'TASNAME  THEN TEST MUST BE FOR                        
         BNH   RVAL990             16 + L'TASNAME                               
         B     RVAL950                                                          
RVAL930  CLI   0(R4),8+L'TASNAME                                                
         BNH   *+8                                                              
RVAL950  LA    R3,TANAELQ          FIELD IS LONG ENOUGH FOR FULL NAME           
         SPACE 1                                                                
RVAL990  GOTO1 CHAROUTL,DMCB,(X'80',(R3)),(1,(R4))                              
         SPACE 1                                                                
RVALX    MVI   ERROR,0                                                          
         B     YES                                                              
         SPACE 2                                                                
RVALERR3 TM    TGBYTE,X'80'        IF SCREEN DATA                               
         BO    *+8                                                              
         NI    4(R2),X'DF'         TURN OFF PREVIOUSLY VALIDATED                
RVALERR1 MVI   ERROR,NOTFOUND      SET RECORD NOT FOUND ERROR                   
         B     *+8                                                              
RVALERR2 MVI   ERROR,MISSING       SET MISSING INPUT FIELD ERROR                
         SPACE 1                                                                
         TM    TGBYTE,X'04'        DOES USER WANT ERRORS RETURNED               
         BZ    THEEND                                                           
         B     NO                  AND CC NE                                    
         SPACE 2                                                                
CLCTOGLB CLC   WORK(0),0(R4)       COMPARE GLOBAL STORAGE TO WORK               
CLCGLBSP CLC   0(0,R4),SPACES      COMPARE GLOBAL STORAGE TO SPACES             
MVCTOGLB MVC   0(0,R4),WORK        MOVE FLD FROM WORK TO GLOBAL STORAGE         
MVCTOSCR MVC   8(0,R2),0(R4)       MOVE FLD FROM GLOBAL STORAGE TO SCRN         
MVCTOKEY MVC   0(0,R3),0(R4)       MOVE FLD FROM GLOBAL STORAGE TO KEY          
CMPLMKEY XC    0(0,R3),HEXFFS      COMPLEMENT FIELD IN KEY                      
         SPACE 1                                                                
KEYCOMP  CLC   KEY(0),KEYSAVE      COMPARE CURRENT KEY TO SAVED KEY             
         EJECT                                                                  
*              ROUTINE TO SET RF EQUAL TO LENGTH FOR KEY COMPARE                
         SPACE 1                                                                
SETLCMP  DS    0H                                                               
         LA    RF,L'TLDRKEY-1      SET L'COMPARE=L'DIRECTORY KEY                
         CLI   KEYSAVE,TLW2CDQ     FOR W2 KEY                                   
         BNE   *+8                                                              
         LA    RF,(TLW2CDTE-TLW2D)-1 CHECK ONLY UP TO PRINT DATE                
         CLI   KEYSAVE,TLCOICDQ    FOR COMMERCIAL ID KEY                        
         BNE   *+8                                                              
         LA    RF,(TLCOICOM-TLCOPD)-1 CHECK ONLY UP TO INTERNAL NUMBER          
         CLI   KEYSAVE,TLCKCCDQ    FOR CHECK NUMBER KEY                         
         BNE   *+8                                                              
         LA    RF,(TLCKCBNK-TLCKPD)-1 CHECK ONLY UP TO BANK                     
         CLI   KEYSAVE,TLINHCDQ    FOR INV PAYMENT HIST KEY                     
         BNE   *+8                                                              
         LA    RF,(TLINHSEQ-TLINPD)-1 CHECK ONLY UP TO SEQUENCE                 
         CLI   KEYSAVE,TLECCDQ     FOR ECAST KEY                                
         BNE   *+8                                                              
         LA    RF,(TLECINV-TLECD)-1   CHECK ONLY UP TO INVOICE NUMBER           
         CLI   KEYSAVE,TLECCCDQ     FOR ECAST SSN KEY                           
         BNE   *+8                                                              
         LA    RF,(TLECCINV-TLECPD)-1  CHECK ONLY UP TO INVOICE NUMBER          
         CLI   KEYSAVE,TLAKCDQ      FOR ALIAS KEY                               
         BNE   *+8                                                              
         LA    RF,(TLAKCOM-TLAKD)-1    CHECK ONLY UP TO INTERNAL NUMBER         
         CLI   KEYSAVE,TLCOGCDQ     FOR COMML CLIENT GROUP KEY                  
         BNE   *+8                                                              
         LA    RF,(TLCOGAGY-TLCOPD)-1  CHECK ONLY UP TO CID                     
         CLI   KEYSAVE,TLMTCDQ      FOR CNET/TMKT/RMKT/CSYS KEY                 
         BNE   *+8                                                              
         LA    RF,(TLMTINUM-TLMTCD)-1  CHECK ONLY UP TO INTERNAL                
         CLI   KEYSAVE,TLMTALDQ     FOR TMKT ALPHA KEY                          
         BNE   *+8                                                              
         LA    RF,(TLMTALIN-TLMTPD)-1  CHECK ONLY UP TO INTERNAL                
         CLI   KEYSAVE,TLMTICDQ     FOR CNET/MKT/CSYS INTERNAL KEY              
         BNE   *+8                                                              
         LA    RF,(TLMTICCD-TLMTPD)-1  CHECK ONLY UP TO CODE                    
*                                                                               
         CLI   KEYSAVE,TLPMCDQ      X'24' = SPECIAL RECORDS                     
         BNE   SETLCMP3                                                         
         CLI   KEYSAVE+1,TLT4SCDQ   FOR T4 SUB CODE                             
         BE    SETLCMP2                                                         
         CLI   KEYSAVE+1,TLR1SCDQ   FOR RL-1 SUB CODE                           
         BE    SETLCMP2                                                         
         CLI   KEYSAVE+1,TLTASCDQ   FOR T4A-NR SUB CODE                         
         BE    SETLCMP2                                                         
         CLI   KEYSAVE+1,TLN4SCDQ   FOR NR4 SUB CODE                            
         BNE   *+8                                                              
SETLCMP2 LA    RF,(TLT4SSN-TLT4D)+8 CHECK UP TO SSN                             
SETLCMP3 DS    0H                                                               
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE INVALIDATES ALL KEY FIELDS AFTER CURRENT                 
         SPACE 1                                                                
*                                  R2=A(CURRENT FIELD HEADER)                   
INVALSCR NTR1                                                                   
INVLS1   NI    4(R2),X'DF'         TURN OFF VALIDITY BIT                        
         CLI   MODE,VALKEY         IF MODE IS VALIDATE KEY                      
         BE    INVLS2                                                           
         CLI   GCMODE,C'4'         OR IF PROG REC                               
         BNE   XIT                                                              
         CLI   MODE,VALREC         AND MODE IS VALIDATE RECORD                  
         BNE   XIT                                                              
INVLS2   BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         BE    XIT                 EOS                                          
         BAS   RE,ONEBPROT         IS IT 1 BYTE PROTECTED                       
         BE    XIT                 YES                                          
         BH    INVLS2              NO, BUT IT'S PROTECTED                       
         B     INVLS1              IT'S UNPROTECTED - INVALIDATE IT             
         SPACE 3                                                                
*              ROUTINE BUMPS TO THE NEXT SCREEN FIELD                           
         SPACE 1                                                                
*                                  R2=A(CURRENT FIELD HEADER)                   
BUMP     DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0             RETURN CC EQ IF END OF SCREEN                
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE DETERMINES WHETHER REACHED 1 BYTE PROT. FIELD            
         SPACE 1                                                                
*                                  R2=A(CURRENT FIELD HEADER)                   
ONEBPROT DS    0H                                                               
         TM    1(R2),X'20'         IF FIELD IS UNPROTECTED                      
         BO    *+10                                                             
         CLI   0(R2),X'FF'         RETURN CC LOW                                
         BR    RE                                                               
         CLI   0(R2),9             RETURN CC EQ IF 1 BYTE PROT. FIELD           
         BR    RE                  RETURN CC HIGH IF PROTECTED                  
         EJECT                                                                  
*              ROUTINE HANDLES CALLS TO RECVAL WITH AN FLIST                    
         SPACE                                                                  
FLIST    NTR1                                                                   
         TM    TGBYTE,X'80'        MUST BE ON SCREEN                            
         BO    NO                                                               
         CLI   8(R2),C'@'          AND START WITH @                             
         BNE   NO                                                               
         MVI   TGLTYP,TLGLTYPF     SET FOR FLIST RECORD VALIDATION              
         MVC   TGLST,SPACES                                                     
         ZIC   R1,5(R2)            L'INPUT                                      
         AHI   R1,-2               - 1 FOR @, - 1 FOR EXEC MOVE                 
         BM    FLDINV              INVALID IF ONLY @ INPUT                      
         CLM   R1,1,=AL1(L'TGLST)                                               
         BL    *+6                                                              
         DC    H'0'                DIE IF INPUT TOO BIG FOR TGLST               
         SPACE                                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGLST(0),9(R2)                                                   
         OI    TGBYTE,X'80'        SET NON-SCREEN DATA AND PRESERVE             
         GOTO1 RECVALL,DMCB,TLGLCDQ,(TGBYTE,0)  REST OF DMCB                    
         BNE   THEEND                                                           
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE A MEDIA CODE                                 
*                                                                               
*                                  P1 = A(5-BYTE WORK AREA) OR A(EQU)           
*                                  P1 BYTE 0  X'80' = P1 IS EQUATE              
         SPACE 1                                                                
MEDVALL  NTR1                                                                   
MEDVAL   BRAS  RE,MVAL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE A USE CODE AND/OR TYPE                       
*                                                                               
*                                  P1 = A(3-BYTE USE CODE) OR A(EQU)            
*                                  P1 BYTE 0  X'80' = P1 IS EQUATE              
*                                             X'40' = DON'T SET TYPE            
*                                             X'10' = P2 IS CHAR.               
*                                  P2 = A(EQU) OR A(3-BYTE USE TYPE)            
         SPACE 1                                                                
USEVAL   DS    0H                                                               
         BRAS  RE,USEVALR                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE AN ACTRA COMMERCIAL TYPE                     
*                                                                               
*                                     BYTE  0   = X'80' = EQUATE                
*                                     BYTES 1-3 = A(4 BYTE COMML TYPE)          
         SPACE 1                                                                
CCTYPVAL BRAS  RE,CCTYVAL                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE A COMMERCIAL TYPE                            
*                                                                               
*                                  P1 = A(1 BYTE EQUATE)                        
CTYPVALL NTR1                                                                   
CTYPVAL  DS    0H                                                               
         XC    TGCTEQU,TGCTEQU     CLEAR GLOBAL FIELDS                          
         XC    TGCTNAME,TGCTNAME                                                
*                                                                               
         L     R2,0(R1)            COMMERCIAL TYPE                              
         L     R3,TGACOMT                                                       
         USING CTYD,R3                                                          
*                                                                               
CTY10    CLI   0(R3),X'FF'                                                      
         BE    NO                                                               
         CLC   CTYEQU,0(R2)        MATCH ON EQUATE                              
         BE    CTY50                                                            
         LA    R3,CTYNEXT          BUMP R3 TO NEXT COMMERCIAL CODE              
         B     CTY10                                                            
*                                                                               
CTY50    MVC   TGCTEQU,CTYEQU      SAVE EQUATE IN GLOBAL                        
         MVC   TGCTNAME,CTYNAME    SAVE NAME IN GLOBAL                          
*                                                                               
         B     YES                 RETURN EQ                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE A STAFF CODE                                 
         SPACE 1                                                                
*                                  P1 = A(1-BYTE STAFF CODE)                    
*                                  P2 = A(ACCESS LEVEL BITS) OR ZERO            
*                                       RETURNS CC EQ IF ACCESSIBLE             
         SPACE 1                                                                
STAFVALL NTR1  ,                   ENTRY POINT FOR LOCAL CALLS                  
STAFVAL  DS    0H                                                               
         LM    R3,R4,0(R1)                                                      
         SPACE 1                                                                
         L     R2,TGASTAFS         R2=A(STAFF TABLE)                            
         USING STAFTABD,R2                                                      
         SPACE 1                                                                
STV2     CLI   0(R2),X'FF'                                                      
         BE    NO                                                               
         CLC   STAFEQU,0(R3)       MATCH ON CODE                                
         BE    *+12                                                             
         LA    R2,STAFNEXT                                                      
         B     STV2                                                             
         SPACE 1                                                                
         LTR   R4,R4               TEST ACCESS CHECK REQUESTED                  
         BZ    STV6                                                             
         MVC   TGBYTE,0(R4)        EXISTING ACCESS LEVEL                        
         OC    TGBYTE,STAFLVL      MERGE WITH NEW ACCESS LEVEL                  
         CLC   TGBYTE,0(R4)        IF EXISTING ACCESS CHANGED                   
         BNE   NO                  THEN REJECT - RETURN CC NE                   
         SPACE 1                                                                
         CLI   STAFEQU,TASTTYPP    EXCEPT FOR PROGRAMMERS                       
         BE    YES                                                              
         CLC   STAFLVL,0(R4)       CANNOT ACCESS THOSE AT SAME LEVEL            
         BNE   YES                                                              
                                                                                
******** CLI   STAFEQU,TASTTYP2    ALLOW FOR SYSTEM MANAGERS                    
******** BE    YES                                                              
         B     NO                                                               
         SPACE 1                                                                
STV6     MVC   TGSTEQU,STAFEQU     SAVE CODE IN GLOBAL                          
         MVC   TGSTDSP,STAFDSP          DISP. TO SECURITY MASK BYTE             
         MVC   TGSTBIT,STAFBIT          CORRES. BIT IN MASK                     
         MVC   TGSTLVL,STAFLVL          BITS CORRES. TO REL. ACCESS LVL         
         MVC   TGSTNAME,STAFNAME        NAME                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE PASSIVE POINTERS                                 
         SPACE 1                                                                
*                                  P1=A(BLOCK TO SAVE POINTERS IN)              
SAVPTRS  DS    0H                                                               
         L     R3,0(R1)                                                         
         XC    0(L'TLDRREC+1,R3),0(R3) PRE-CLEAR START OF BLOCK                 
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       DON'T BOTHER IF ACTION ADD                   
         BNE   *+12                                                             
         TM    WHENOK,X'01'        UNLESS TREATING ADD AS CHANGE                
         BZ    SAVPX                                                            
         CLI   MODE,RECREST        ALSO DON'T BOTHER IF RESTORE                 
         BE    SAVPX                                                            
         SPACE 1                                                                
         GOTO1 GENPTRSL,DMCB,(0,(R3))  BUILD THE POINTERS                       
SAVPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD/CHANGE PASSIVE POINTERS                           
         SPACE 1                                                                
*                                  P1, BYTE 0 X'80'= PROCESS ACTIVE PTR         
*                                             X'40'= TREAT LIKE DELETE          
*                                             X'20'= FORCE CHANGE ALL           
*                                             X'10'= TRACE (OFFLINE)            
*                                             X'08'= P2 HAS A(NEW BLK)          
*                                             X'04'= OK TO DELETE TLINB         
*                                             X'02'= OK TO REPLACE NON-         
*                                                    DEL'D PTRS (CHKS)          
*                                             X'01'= FORCE SEQ BASED ON         
*                                                    TIME (PASSIVE PTR)         
ADDPTRS  L     R2,0(R1)            P1, BYTES 1-3 = A(ORIGINAL POINTERS)         
         L     R5,4(R1)            P2 = A(NEW POINTER BLOCK) (OPTIONAL)         
         MVC   TGBYTE,0(R1)        SAVE HOB OF PARAMETER 1                      
         SPACE 1                                                                
         TM    TGBYTE,X'08'        IF NEW POINTER BLOCK NOT PASSED              
         BO    *+8                                                              
         LA    R5,BLOCK            USE GENERAL AREA                             
         SPACE 1                                                                
         TM    TGBYTE,X'80'        UNLESS REQUESTED OTHERWISE                   
         BO    *+8                                                              
         LA    R2,L'TLDRREC(R2)    SKIP ACTIVE POINTER                          
         SPACE 1                                                                
         XC    0(L'TLDRREC+1,R5),0(R5)  PRE-CLEAR NEW POINTERS BLOCK            
         SPACE 1                                                                
         TM    TGBYTE,X'40'        UNLESS SPECIFICALLY REQ. OTHERWISE           
         BO    ADDP1                                                            
         CLI   MODE,XRECDEL        OR ACTION IS DELETE                          
         BE    ADDP1                                                            
         GOTO1 GENPTRSL,DMCB,(TGBYTE,(R5))  BUILD NEW POINTER BLOCK             
         SPACE 1                                                                
ADDP1    TM    TGBYTE,X'20'        IF FORCE CHANGE ALL PTRS                     
         BNO   *+8                                                              
         BAS   RE,DELPTRS          THEN MARK OLD ONES DELETED                   
         SPACE 1                                                                
         LR    R3,R5                                                            
         TM    TGBYTE,X'80'        IF WE'RE PROCESSING ACTIVE POINTER           
         BZ    *+12                                                             
         BAS   RE,ACTVPTR          SPECIAL HANDLING IF IT CHANGED               
         BE    *+8                                                              
         LA    R3,L'TLDRREC(R3)    SKIP ACTIVE POINTER                          
         USING TLDRD,R3            R3=A(FIRST NEW POINTER)                      
         SPACE 1                                                                
ADDP2    LR    R4,R2               R4=A(FIRST OLD POINTER)                      
         CLI   0(R3),0                                                          
         BE    ADDP10              NO MORE NEW POINTERS                         
ADDP3    CLI   0(R4),0             SEE IF NEW POINTER ALREADY EXISTS            
         BE    ADDP6                                                            
         CLC   TLDRCD,0(R4)        FIRST MATCH ON POINTER TYPE                  
         BE    *+12                                                             
ADDP4    LA    R4,L'TLDRREC(R4)    BUMP TO NEXT OLD POINTER                     
         B     ADDP3                                                            
         SPACE 1                                                                
         BAS   RE,SETLKEY          SET RF=LENGTH FOR COMPARE                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TLDRKEY(0),0(R4)    IF KEY MATCHES                               
         BNE   ADDP4                                                            
         MVI   0(R4),X'FF'         MARK OLD KEY USED AND GET NEXT NEW           
         B     ADDP8                                                            
         SPACE 1                                                                
ADDP6    BAS   RE,ADDPTR           ADD NEW POINTER                              
         SPACE 1                                                                
ADDP8    LA    R3,L'TLDRREC(R3)    BUMP TO NEXT NEW ONE                         
         B     ADDP2                                                            
         SPACE 1                                                                
ADDP10   BAS   RE,DELPTRS          DELETE ALL UNUSED OLD POINTERS               
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES ACTIVE KEY CHANGING                              
         SPACE 1                                                                
*                                  R2=A(OLD POINTER BLOCK)                      
         USING TLDRD,R3            R3=A(NEW POINTER BLOCK)                      
ACTVPTR  NTR1                                                                   
         CLI   0(R2),0             IF NO OLD POINTER (ADDING)                   
         BE    ACPX                DON'T BOTHER                                 
         CLI   0(R2),X'FF'         OR IF ALREADY MARKED USED                    
         BE    ACPX                DON'T BOTHER                                 
         SPACE 1                                                                
         BAS   RE,SETLKEY          SET RF=L'COMPARE                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TLDRKEY(0),0(R2)    TEST ACTIVE KEY CHANGED                      
         BE    ACPX                NO - RETURN CC EQ                            
         SPACE 1                                                                
         BAS   RE,DELPTRS          DELETE ALL OLD POINTERS                      
         SPACE 1                                                                
         OC    DMWORK+12(12),DMWORK+12  IF RECORD HAS BEEN EXTENDED             
         BZ    ACP2                                                             
         MVI   RDUPDATE,C'Y'       THEN REREAD IT FOR UPDATE                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
ACP2     L     R4,AIO              R4=A(FILE RECORD)                            
         USING TLRCD,R4                                                         
         MVC   TLRCKEY(L'TLRCKEY-1),0(R2) INSURE REC HAS OLD ACTIVE KEY         
         MVC   TLRCCD,TLDRCD       INSURE WE HAVE CORRECT RECORD CODE           
         OI    TLRCSTAT,X'80'      MARK RECORD DELETED                          
         GOTO1 PUTREC              AND WRITE IT BACK                            
         BAS   RE,TRACEREC                                                      
         MVC   TLRCKEY,TLDRKEY     MOVE NEW ACTIVE PTR BACK INTO REC            
         NI    TLRCSTAT,X'7F'      UN-MARK THE RECORD                           
         SPACE 1                                                                
         BAS   RE,CHKPTR           CHECK IF NEW ACTV PTR EXISTS DELETED         
         BE    ACP4                                                             
         GOTO1 ADDREC              IF NOT ADD A NEW ONE                         
         BAS   RE,TRACEREC                                                      
         B     NO                  RETURN CC NE - DON'T PROCESS ACTIVE          
         SPACE 1                                                                
ACP4     OI    DMINBTS,X'08'       YES - SET READ DELETED                       
         MVI   RDUPDATE,C'Y'             AND READ FOR UPDATE                    
         MVC   AIO,AIO2            USE ALTERNATE I/O AREA                       
         GOTO1 GETREC              GET THE RECORD                               
         ST    R4,AIO              RESET I/O AREA                               
         GOTO1 PUTREC              AND WRITE BACK NEW RECORD                    
         BAS   RE,TRACEREC                                                      
         SPACE 1                                                                
ACPX     B     YES                 RETURN CC EQ - PROCESS ACTIVE PTR            
         EJECT                                                                  
*              LOCAL ROUTINE TO DELETE ALL UNUSED PASSIVE POINTERS              
         SPACE 1                                                                
*                                  R2=A(OLD POINTER BLOCK)                      
DELPTRS  NTR1                                                                   
DELP2    CLI   0(R2),0             END OF LIST                                  
         BE    DELPX                                                            
         TM    TGBYTE,X'04'        UNLESS SPECIFICALLY REQUESTED                
         BO    DELP3                                                            
         CLI   MODE,XRECDEL        OR SPECIFICALLY DELETING RECORD              
         BE    DELP3                                                            
         CLI   0(R2),TLINBCDQ      NEVER DELETE INVOICE STATUS POINTER          
         BE    DELP7                                                            
         SPACE 1                                                                
DELP3    CLI   0(R2),X'FF'         TEST MARKED USED ALREADY                     
         BE    DELP8                                                            
         XC    KEY,KEY                                                          
         LR    R3,R2               SET R3=A(OLD KEY ALSO)                       
         BAS   RE,SETLKEY          SET L'KEY W/O SEQ. NUMBER                    
         EX    RF,MVCKEY           MOVE IN KEY                                  
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                LOOK FOR IT                                  
         SPACE 1                                                                
DELP4    BAS   RE,SETLKEY          SET L'KEY W/O SEQ. NUMBER                    
         EX    RF,CLCKEY           COMPARE KEY TO KEYSAVE                       
         BE    *+14                                                             
         TM    TGBYTE,X'02'        TEST OK TO NOT FIND                          
         BO    DELP7                                                            
         DC    H'0'                MUST BE ON FILE                              
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         CLC   TLDRDA,DMDSKADD     MUST BE CORRECT DISK ADDRESS                 
         BE    DELP6                                                            
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 ELSE READ NEXT                               
         LR    R3,R2               RESET R3=A(OLD KEY)                          
         B     DELP4                                                            
         SPACE 1                                                                
DELP6    OI    TLDRSTAT,X'80'      MARK IT DELETED                              
         SPACE 1                                                                
         CLI   KEY,TLNXCDQ         IF KEY IS FOR A HOLD RECORD                  
         BL    DELP6A                                                           
         CLI   KEY,TLNXNCDQ                                                     
         BH    DELP6A                                                           
         OI    TLDRSTAT,TLNXSDEL   TURN ON SPECIAL DELETE BIT                   
         SPACE 1                                                                
DELP6A   GOTO1 WRITE               AND WRITE IT BACK                            
         BAS   RE,TRACEPTR         TRACE IF NECESSARY                           
         SPACE 1                                                                
DELP7    MVI   0(R2),X'FF'         MARK OLD KEY USED                            
         SPACE 1                                                                
DELP8    LA    R2,L'TLDRREC(R2)                                                 
         B     DELP2               TRY NEXT                                     
         SPACE 1                                                                
DELPX    L     R4,AIO              R4=A(FILE RECORD)                            
         USING TLRCD,R4                                                         
         MVC   TLRCKEY,0(R5)       RESTORE ACTIVE KEY TO RECORD                 
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ROUTINE TO ADD A PASSIVE POINTER                           
         SPACE 1                                                                
         USING TLDRD,R3            R3=A(NEW KEY)                                
ADDPTR   NTR1                                                                   
         CLI   TLDRCD,TLINBCDQ     IF ADDING/CHANGING INV. STATUS PTR.          
         BNE   *+8                                                              
         NI    TGBYTE,X'FB'        TURN OFF OK TO DELETE BIT                    
         SPACE 1                                                                
         BAS   RE,CHKPTR           TEST WHETHER IT EXISTS DELETED               
         BNE   ADP2                                                             
         MVC   TLDRKEY,KEY         YES - INSURE EXACT KEY IN BLOCK              
         L     RF,WRITE            SET TO WRITE                                 
         B     ADP4                                                             
         SPACE 1                                                                
ADP2     CLI   MODE,XRECREST       NO - IF WE'RE RESTORING                      
         BNE   *+6                                                              
         DC    H'0'                MUST FIND DELETED RECORD                     
         L     RF,ADD              SET TO ADD                                   
         SPACE 1                                                                
ADP4     MVC   TLDRDA,DMDSKADD     SET DISK ADDRESS IN NEW ONE                  
         MVC   KEY,TLDRREC         MOVE THE RECORD BACK TO KEY                  
         BASR  RE,RF               ADD/WRITE THE RECORD                         
         BAS   RE,TRACEPTR         TRACE IF NECESSARY                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF DIRECTORY RECORD ON FILE DELETED           
         SPACE 1                                                                
         USING TLDRD,R3            R3=A(DIRECTORY RECORD)                       
CHKPTR   NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         BAS   RE,SETLKEY          SET L'KEY W/O SEQ. NUMBER                    
         EX    RF,MVCKEY           MOVE IN KEY                                  
         OI    DMINBTS,X'08'       READ DELETED                                 
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
         SPACE 1                                                                
CKP2     NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         BAS   RE,SETLKEY2         SET L'KEY FOR COMPARE                        
         EX    RF,CLCKEY           DID WE FIND A MATCH                          
         BNE   NO                                                               
         TM    KEY+TLDRSTAT-TLDRD,X'80'  YES - IF IT'S MARKED DELETED           
         BO    YES                       RETURN CC EQ                           
         CLI   TLDRCD,TLINBCDQ     OK TO WRITE OVER INV. STATUS PTRS.           
         BE    YES                                                              
         TM    TGBYTE,X'02'        ALSO OK IF SPEC REQUESTED TO DO SO           
         BO    YES                                                              
         SPACE 1                                                                
         OI    DMINBTS,X'08'       ELSE SET TO READ FOR ANOTHER                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     CKP2                                                             
         SPACE 3                                                                
MVCKEY   MVC   KEY(0),TLDRKEY      ** EXECUTED KEY MOVE **                      
CLCKEY   CLC   KEY(0),KEYSAVE      ** EXECUTED KEY COMPARE **                   
         EJECT                                                                  
*              LOCAL ROUTINE TO SET L'KEY FOR COMPARES/MOVES                    
         SPACE 1                                                                
         USING TLDRD,R3            R3=A(KEY)                                    
SETLKEY  DS    0H                                                               
         CLI   TLDRCD,TLINBCDQ     SPECIAL FOR INVOICE STATUS PTRS.             
         BNE   SETLKEY2                                                         
         LA    RF,TLDRSTAT+L'TLDRSTAT-TLDRD-1  INCL. STATUS BYTES               
         BR    RE                                                               
         SPACE 1                                                                
SETLKEY2 DS    0H                                                               
         LA    RF,L'TLDRKEY-1      ELSE SET TO FULL KEY                         
         BR    RE                  RETURN RF=LENGTH FOR EXEC. COMPARE           
         EJECT                                                                  
*              ROUTINE TO TRACE ADDPTR DIRECTORY CHANGES                        
         SPACE 1                                                                
TRACEPTR NTR1                                                                   
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   TRAPX                                                            
         TM    TGBYTE,X'10'        TEST TRACE REQUESTED                         
         BZ    TRAPX                                                            
         ZIC   R2,TGBYTE           TRACE CREAMS TGBYTE                          
         L     R3,DMCB             S/B P1 OF DATAMGR CALL TO GENCON             
         GOTO1 TRACEL,DMCB,KEY,L'TLDRREC,(R3),5                                 
         STC   R2,TGBYTE                                                        
TRAPX    B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TRACE ADDPTR RECORD CHANGES                           
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   TRARX                                                            
         TM    TGBYTE,X'10'        TEST TRACE REQUESTED                         
         BZ    TRARX                                                            
         ZIC   R2,TGBYTE           TRACE CREAMS TGBYTE                          
         L     R3,DMCB             S/B P1 OF DATAMGR CALL TO GENCON             
         GOTO1 TRACEL,DMCB,AIO,0,(R3),6                                         
         STC   R2,TGBYTE                                                        
TRARX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GENERATES PASSIVE POINTER BLOCK                          
         SPACE 1                                                                
*                                  P1=A(POINTER BLOCK)                          
*                                  P1 BYTE 0 X'01' (LOCAL CALLS ONLY)           
*                                                 -SEQ BASED ON TIME            
GENPTRSL NTR1  ,                   ENTRY POINT FOR LOCAL CALLS                  
         MVI   BYTE,0              PRE-CLEAR BYTE                               
         TM    0(R1),X'01'                                                      
         BZ    *+8                                                              
         OI    BYTE,X'20'          SET FORCING SEQ BASED ON TIME                
         B     GENPTRS5                                                         
         SPACE 1                                                                
GENPTRS  DS    0H                                                               
         MVI   BYTE,0              PRE-CLEAR BYTE                               
         SPACE 1                                                                
GENPTRS5 L     R3,0(R1)                                                         
         LA    R4,TGPTRBLK         BUILD PARAMETER BLOCK                        
         USING CPTRD,R4                                                         
         MVC   CPDATCON,DATCON                                                  
         MVC   CPADDAY,ADDAY                                                    
         MVC   CPCATTAB,TGACATS    A(CATEGORY TABLE)                            
         MVC   CPYRSTAB,TGAYEARS   A(CONTRACT YEARS)                            
         MVC   CPPROTOF,PROTOFF    A(PROTOFF)                                   
         MVC   CPPROTON,PROTON     A(PROTON)                                    
         MVC   CPUSETAB,TGAUSES    A(USE TABLE)                                 
         SPACE 1                                                                
         OI    BYTE,X'40'          SET PASSING PARAM BLOCK                      
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         OI    BYTE,X'80'          SET ONLINE                                   
         SPACE 1                                                                
         CLI   ACTNUM,ACTREIS      IF ACTION REISSUE                            
         BE    GENPTRS6                                                         
         CLI   ACTNUM,ACTMREI      OR MULTIPLE REISSUE                          
         BE    GENPTRS6                                                         
         CLI   ACTNUM,ACTVOID      OR VOID                                      
         BNE   *+8                                                              
GENPTRS6 OI    BYTE,X'10'          DO NOT GENERATE STOP/PULL PTRS               
         SPACE 1                                                                
         TM    TGGNSTAT,TGJSTCOE   IF ONLY GENERATE TLCOECDQ POINTER            
         BZ    *+8                                                              
         OI    BYTE,X'08'          SET STATUS                                   
         SPACE 1                                                                
         GOTO1 TGTALDCP,DMCB,(BYTE,AIO),(R3),(R4),,DMDSKADD  BLD PTRS           
         B     XIT                                                              
         EJECT                                                                  
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA) OR ZERO FOR RECORD                               
*        PARAMETER 3 - A(LABEL) OR ZERO FOR NO LABEL                            
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
TRACEL   NTR1  ,                   ENTRY POINT FOR LOCAL CALLS                  
TRACE    DS    0H                                                               
         MVC   BYTE,ELCODE         SAVE ELCODE IN BYTE                          
*                                                                               
         MVC   DUB(4),HEADHOOK     SAVE A(HOOK ROUTINES)                        
         MVC   DUB+4(4),MIDHOOK                                                 
         MVC   TGBYTE,FOOTLNS                                                   
         XC    HEADHOOK,HEADHOOK   CLEAR UNTIL ROUTINE IS DONE                  
         XC    MIDHOOK,MIDHOOK                                                  
         MVI   FOOTLNS,0                                                        
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         L     RF,TWADCONS         R0 = A(PRNTBL)                               
         USING TWADCOND,RF                                                      
         L     R0,TPRNTBL                                                       
         DROP  RF                                                               
*                                                                               
         BAS   RE,PRNTIT           PRINT ONE BLANK LINE FIRST                   
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         BAS   RE,PRNTIT           PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 (R0),DMCB,0,(R2),C'DUMP',(R3),=X'01C4',(C'P',TWAVPRNT)           
*                                                                               
TR15     LR    R3,R2               A(RECORD)                                    
         AH    R3,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R3)            PRINT ELEMENT                                
         GOTO1 (R0),DMCB,0,(R3),C'DUMP',(R4),=X'01C4',(C'P',TWAVPRNT)           
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 (R0),DMCB,0,(R2),C'DUMP',(R3),=X'01C4',(C'P',TWAVPRNT)           
*                                                                               
TR100    DS    0H                                                               
*                                                                               
         MVC   ELCODE,BYTE         RESTORE ELCODE                               
         MVC   HEADHOOK,DUB        RESTORE A(HOOK ROUTINES)                     
         MVC   MIDHOOK,DUB+4                                                    
         MVC   FOOTLNS,TGBYTE                                                   
*                                                                               
TRX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET NAME FROM EXTRA RECORD FOR LISTS                  
*                                                                               
*                                  P1 = RECORD EQUATE                           
*                                  P1 BYTE 0 = X'80' DO NOT RE-READ             
*                                              KEY PASSED                       
*                                  P2 = A(PLACE TO PUT 16-BYTE NAME)            
*                                  P3 = A(KEY TO RESTORE)                       
         SPACE 1                                                                
XNAME    DS    0H                                                               
         MVC   BYTE,0(R1)          TEST - RE-READ PREV RECORD                   
         MVC   TGXARG,3(R1)        SAVE RECORD CODE                             
         LM    R2,R3,4(R1)         R2 = A(PLACE TO PUT 16-BYTE NAME)            
         MVC   0(16,R2),SPACES     PRE-CLEAR TO SPACES                          
         MVC   TGNAME(32),0(R3)    SAVE KEY IN NAME FIELD, FOR NOW              
*                                                                               
         L     R4,0(R1)            CALL RECVAL TO GET RECORD                    
         GOTO1 RECVALL,DMCB,(R4),(X'A0',0)                                      
         MVC   KEY(32),TGNAME      (RESTORE KEY IN CASE NEED TO READ)           
         BNE   XNAMEX              XIT IF RECORD NOT FOUND                      
*                                                                               
         CLI   TGXARG,TLCOCCDQ     IF COMMERCIAL ID REQUESTED                   
         BNE   XNAME10                                                          
         L     R3,AIO              THEN GET COMMERCIAL DETAILS ELEMENT          
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XNAMEX                                                           
         USING TACOD,R3                                                         
         MVC   0(12,R2),TACOCID    RETURN COMMERCIAL ID                         
         B     XNAMEX                                                           
*                                                                               
XNAME10  DS     0H                 ELSE GET SHORT NAME                          
         GOTO1  CHAROUTL,DMCB,(X'80',TASNELQ),0                                 
         MVC    0(16,R2),TGNAME    RETURN SHORT NAME                            
*                                                                               
XNAMEX   TM    BYTE,X'80'          DO NOT RE-READ PASSED KEY                    
         BO    XNX10                                                            
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
XNX10    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT GLOBAL VALUES FROM RECORDS                    
         SPACE 1                                                                
EXTRACT  DS    0H                                                               
         L     R3,AIO              R3=A(RECORD)                                 
         USING TLRCD,R3                                                         
         LA    R5,RECTAB           R5=A(RECORD TABLE)                           
         USING RECD,R5                                                          
EXTR4    CLI   0(R5),X'FF'                                                      
         BE    XIT                                                              
         CLC   TLRCCD,RECCD        MATCH ON RECORD CODE                         
         BE    EXTR6                                                            
EXTR5    ZIC   RE,RECLEN           BUMP TO NEXT RECORD IN TABLE                 
         AR    R5,RE                                                            
         B     EXTR4                                                            
         SPACE 1                                                                
EXTR6    ZIC   R0,RECNFLDS         R0=N'FIELDS IN TABLE                         
         LA    RF,RECFLDS          RF=A(FIRST FIELD ENTRY)                      
         USING FLDD,RF                                                          
         SPACE 1                                                                
EXTR8    ZIC   R2,FLDKDSP                                                       
         AR    R2,R3               R2=A(FIELD IN RECORD)                        
         XR    R4,R4                                                            
         ICM   R4,3,FLDGDSP                                                     
         LA    R4,TGD(R4)          R4=A(FIELD IN GLOBAL STORAGE)                
         ZIC   R1,FLDLEN           R1=L'FIELD-1                                 
         EX    R1,OCRECFLD         TEST FIELD IN RECORD                         
         BZ    EXTR9                                                            
         EX    R1,MVCFRREC         MOVE FROM RECORD TO GLOBAL STORAGE           
         LA    RE,TGEPI            IF HANDLING GLOBAL EPISODE NUMBER            
         CR    RE,R4                                                            
         BNE   *+8                                                              
         EX    R1,CMPLMGLB         NEED TO COMPLEMENT FIELD IN GLOBAL           
*                                                                               
EXTR9    LA    RF,FLDNEXT          BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,EXTR8                                                         
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ      IF COMMERCIAL DETAILS EL. PRESENT            
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACOD,R3                                                         
         MVC   TGCID,TACOCID       GET COMMERCIAL ID FROM ELEMENT               
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 3                                                                
OCRECFLD OC    0(0,R2),0(R2)       TEST FIELD IN RECORD                         
MVCFRREC MVC   0(0,R4),0(R2)       MOVE FLD FROM REC TO GLOBAL STORAGE          
CMPLMGLB XC    0(0,R4),HEXFFS      COMPLEMENT FIELD IN GLOBAL STORAGE           
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*                                  P1, BYTE  0   = L'SEARCH ARGUMENT            
*                                      BYTES 1-3 = A(SEARCH ARGUMENT)           
*                                                                               
GETLL    NTR1  ,                       ENTRY POINT FOR LOCAL CALLS              
GETL     DS    0H                                                               
         L     R3,0(R1)                                                         
         ZIC   R4,0(R1)                                                         
         CLC   =C'TALFIL',SYSFIL                                                
         BNE   *+12                                                             
         LA    RF,=CL8'TALFIL'                                                  
         B     *+8                                                              
         LA    RF,SYSFIL                                                        
         ST    RF,DMCB                                                          
*                                                                               
         MVI   DMCB,C'G'                                                        
         GOTO1 HELLO,DMCB,,(ELCODE,AIO),((R4),(R3))                             
         CLI   12(R1),6                                                         
         BE    GETL10                                                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
GETL10   MVC   TGELEM,12(R1)       SAVE A(ELEMENT)                              
         CLI   12(R1),0            RETURN CC                                    
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*                                  P1, BYTE  0   = L'SEARCH ARGUMENT            
*                                      BYTES 1-3 = A(SEARCH ARGUMENT)           
*                                                                               
DELLL    NTR1  ,                       ENTRY POINT FOR LOCAL CALLS              
DELL     DS    0H                                                               
         L     R3,0(R1)                                                         
         ZIC   R4,0(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,AIO),((R4),(R3))                
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         DC    H'00'                                                            
         SPACE 3                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
ADDL     DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT                             
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ALL ERRORS                            
         EJECT                                                                  
*              ROUTINE TO DIG OUT OVERSCALE INFO FOR USE TYPE                   
         SPACE 1                                                                
*                                  P1 BYTE 0 X'80'=RETURN OV% ONLY              
*                                  P1=A(USE TYPE), P2=A(W/S FOR OUTPUT)         
GETOV1   DS    0H                                                               
         MVC   BYTE,0(R1)          BYTE=PARAM. STATUS                           
         L     R2,0(R1)            R2=A(USE TYPE)                               
         L     R4,4(R1)            R4=A(OUTPUT AREA)                            
         XC    0(4,R4),0(R4)                                                    
         SPACE 1                                                                
         USING USETABD,R5                                                       
         L     R5,TGAUSES                                                       
GTOV1A   CLC   USECDE,0(R2)        LOOK UP USE                                  
         BE    GTOV1B                                                           
         LH    RE,USELEN                                                        
         AR    R5,RE                                                            
         CLI   0(R5),X'FF'                                                      
         BNE   GTOV1A                                                           
         SPACE 1                                                                
GTOV1B   L     R3,AIO                                                           
         MVI   ELCODE,TAOPELQ      FIRST LOOK FOR OVERSCALE PERCENT             
         BAS   RE,GETEL                                                         
         BNE   GTOV6                                                            
         USING TAOPD,R3                                                         
         ZIC   R0,TAOPNUM          R0=N'SUB-ELEMENTS                            
         LA    RE,TAOPSBEL                                                      
         USING TAOPSBEL,RE         RE=A(FIRST SUB-ELEMENT)                      
         SPACE 1                                                                
GTOV2    CLC   TAOPUSE,0(R2)       MATCH ON USE TYPE                            
         BNE   *+14                                                             
         MVC   0(4,R4),TAOPPCT     SET PERCENT OVERSCALE                        
         B     GTOV6               AND GO CHECK FOR AMOUNT                      
         SPACE 1                                                                
         CLC   TAOPUSE,=C'ARE'     APPLIES TO ALL REUSE                         
         BNE   GTOV3                                                            
         TM    USESTAT,SESSION     AND USE IS REUSE                             
         BO    GTOV3                                                            
         MVC   0(4,R4),TAOPPCT     SET PCT OVSCL (& CONTINUE LOOKING)           
         DROP  R5                                                               
         SPACE 1                                                                
GTOV3    CLC   TAOPUSE,SPACES      APPLIES TO ALL USE TYPES                     
         BNE   GTOV4                                                            
         OC    0(4,R4),0(R4)       AND PCT NOT ALREADY SET                      
         BNZ   GTOV4                                                            
         MVC   0(4,R4),TAOPPCT     SET PCT OVSCL (& CONTINUE LOOKING)           
         SPACE 1                                                                
GTOV4    LA    RE,L'TAOPSBEL(RE)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R0,GTOV2                                                         
         SPACE 1                                                                
GTOV6    TM    BYTE,X'80'          IF ONLY PERCENT REQUIRED                     
         BO    GTOV20              GET OUT NOW                                  
         L     R3,AIO                                                           
         MVI   ELCODE,TAOAELQ      NOW LOOK FOR OVERSCALE AMOUNT                
         BAS   RE,GETEL                                                         
         BNE   GTOV20                                                           
         USING TAOAD,R3                                                         
         ZIC   R0,TAOANUM          R0=N'SUB-ELEMENTS                            
         LA    RE,TAOASBEL                                                      
         USING TAOASBEL,RE         RE=A(FIRST SUB-ELEMENT)                      
         SPACE 1                                                                
GTOV8    CLC   TAOAUSE,0(R2)       MATCH ON USE TYPE                            
         BNE   GTOV10                                                           
         MVC   0(4,R4),TAOAAMT     SET AMOUNT                                   
         MVI   0(R1),X'FF'         SET WE HAVE AMOUNT                           
         B     GTOVX                                                            
         SPACE 1                                                                
GTOV10   CLC   TAOAUSE,SPACES      APPLIES TO ALL USE TYPES                     
         BNE   *+14                                                             
         MVC   0(4,R4),TAOAAMT     SET AMOUNT (& CONTINUE LOOKING)              
         MVI   0(R1),X'FF'         SET WE HAVE AMOUNT                           
         SPACE 1                                                                
         LA    RE,L'TAOASBEL(RE)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R0,GTOV8                                                         
         DROP  RE                                                               
         SPACE 1                                                                
GTOV20   DS    0H                                                               
******** BRAS  RE,GTOVMV           MVI AND MVN REQUIRE MORE SEARCHING           
GTOVX    B     XIT                 ON RETURN 0(R1)=X'FF'==>HAVE AMOUNT          
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE BUILDS GUARANTEE/FIXED CYCLE TRACKING RECORDS            
         SPACE 1                                                                
*                                  KEY=BASIC KEY                                
*                                  TGDUB=AMT APPLIED  TGDUB+4=BALANCE           
BLDTRK   DS    0H                                                               
         LM    R2,R3,0(R1)         R2=DISP. TO TRACKING NO. IN KEY              
*                                  R3=A(CHECK RECORD)                           
         SPACE 1                                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                GET LAST LOGICAL TRACKING RECORD             
         NI    DMINBTS,X'F7'                                                    
         NI    KEY+TLDRSTAT-TLDRD,X'7F'  INSURE DELETE BIT IS OFF               
         BCTR  R2,0                                                             
         LA    RF,KEY+1(R2)        RF=A(TRACKING NUMBER IN KEY)                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      IF WE DIDN'T FIND ONE                        
         BE    *+16                                                             
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY AND                     
         MVC   0(2,RF),=2X'FF'     SET DUMMY LAST TRACKING NUMBER               
         SPACE 1                                                                
         LH    R1,0(RF)            LAST TRACKING NUMBER                         
         LCR   R1,R1               UN-COMPLEMENTED                              
         LA    R1,1(R1)            ADD 1                                        
         LCR   R1,R1               COMPLEMENT IT AGAIN                          
         STH   R1,0(RF)            AND SET IN NEW KEY                           
         SPACE 1                                                                
         LR    R2,R3               SAVE A(CHECK RECORD)                         
         MVI   ELCODE,TAPDELQ      FIND PAYMENT DETAILS EL.                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R3                                                            
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         SPACE 1                                                                
         LR    R3,R2               RESTORE A(CHECK RECORD)                      
         MVI   ELCODE,TACAELQ      FIND CAST DETAILS EL.                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R3            R3=A(CAST DETAILS ELEMENT)                   
         SPACE 1                                                                
         L     R2,AIO              R2=A(TRACKING RECORD)                        
         USING TLRCD,R2                                                         
         MVC   TLRCKEY,KEY         SET KEY IN I/O AREA                          
         MVC   TLRCLEN,DATADISP                                                 
         XC    TLRCSTAT(10),TLRCSTAT                                            
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R2,ELEMENT          R2=A(GUARANTEE TRACKING ELEMENT)             
         USING TAGTD,R2                                                         
         MVI   TAGTEL,TAGTELQ                                                   
         MVI   TAGTLEN,TAGTLNQ                                                  
         MVC   TAGTCID,TGCID       COMMERCIAL ID                                
         MVC   TAGTAGY,TGAGY       AGENCY                                       
         MVC   TAGTCAT,TGCAT       CATEGORY                                     
         MVC   TAGTCAM,TACAONOF    ON/OFF CAMERA                                
         MVC   TAGTUNI,TACAUN      UNION                                        
         MVC   TAGTYEAR,TACAYEAR   YEAR                                         
         MVC   TAGTOV,TAPDOV1      OVERSCALE RATE 1                             
         MVC   TAGTOV2,TACAOV2     OVERSCALE RATE 2                             
         MVC   TAGTINV,TGINV       INVOICE NUMBER                               
         MVC   TAGTUSE,TAPDUSE     USE CODE                                     
         MVC   TAGTTYPE,TAPDTYPE   USE TYPE                                     
         MVC   TAGTSTRT,TAPDCYCS   CYCLE DATES                                  
         MVC   TAGTEND,TAPDCYCE                                                 
         MVC   TAGTSTUS,TAPDSTUS   STARTING USE NUMBER                          
         MVC   TAGTUSES,TAPDUSES   N'USES                                       
         MVC   TAGTUNIT,TAPDUNIT   N'UNITS                                      
         MVC   TAGTMAJ,TAPDMAJ     MAJORS                                       
         MVC   TAGTPST1,TAPDPST1   PAYMENT STATUS                               
         L     R1,TAPDPAYI                                                      
         A     R1,TAPDPAYC                                                      
         ST    R1,TAGTPAY          PAYMENT AMOUNT                               
         MVC   TAGTPNH,TAPDPNH     PENSION AND HEALTH                           
         MVC   TAGTCRD,TGDUB       AMOUNT APPLIED                               
         MVC   TAGTBAL,TGDUB+4     BALANCE                                      
         MVC   TAGTAPPL,TAPDAPPL   APPLIED AMOUNT                               
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         SPACE 1                                                                
         GOTO1 ACTVINL,DMCB,0      ADD ACTIVITY ELEMENT                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE GUARANTEE RECORDS                            
*              SETS CC AND ERROR, AND RETURNS A(TAGUELQ ELEMENT) IN             
*              FIRST PARAMETER OF PLIST IF NO ERROR AND CC EQUAL                
         SPACE 1                                                                
*                                  P1=A(CORP CODE)                              
*                                     BYTE 0 X'80'=READ FOR UPDATE              
*                                  P2=A(PAYMENT APPLY DATE)                     
GUARVAL  DS    0H                                                               
         LHI   RF,X'20'                                                         
         TM    0(R1),X'80'                                                      
         BZ    *+8                                                              
         LHI   RF,X'30'                                                         
                                                                                
         LM    R4,R5,0(R1)                                                      
         LR    R2,R1               SAVE A(PLIST) IN R2                          
*                                                                               
         GOTO1 RECVALL,DMCB,TLGUCDQ,((RF),0)  GET GUARANTEE REC.                
         BNE   GRVERR                                                           
*                                                                               
         USING TAGUD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS EL.                    
         BAS   RE,GETEL                                                         
         BNE   GRVERR                                                           
         ST    R3,0(R2)            RETURN ITS ADDRESS IN PLIST                  
*                                                                               
         XR    R0,R0                                                            
*                                                                               
         OC    TAGUAGY,TAGUAGY     IF AGENCY DEFINED                            
         BZ    GRV10                                                            
         CLC   TAGUAGY,TGAGY       TEST CORRECT AGENCY                          
         BE    GRV10                                                            
         LHI   R0,1                                                             
*                                                                               
GRV10    OC    TAGUCLI,TAGUCLI     IF CLIENT DEFINED                            
         BZ    GRV20                                                            
         CLC   TAGUCLI,TGCLI       TEST CORRECT CLIENT                          
         BE    GRV20                                                            
         LHI   R0,1                                                             
*                                                                               
GRV20    OC    TAGUPD,TAGUPD       IF GUARANTEE HAS A PERIOD                    
         BZ    GRV30                                                            
*                                                                               
         MVC   WORK(3),0(R5)       PAYMENT MUST FALL WITHIN GUAR PERIOD         
         OC    WORK(3),WORK                                                     
         BNZ   *+10                                                             
         MVC   WORK(3),TGTODAY1    USE TODAY'S DATE IF NO CYCLE DATES           
*                                                                               
         CLC   WORK(3),TAGUEND     MUST BEGIN ON OR BEFORE GUAR END             
         BH    GRVERRPD                                                         
         CLC   WORK(3),TAGUSTRT    AND ON OR AFTER GUAR START                   
         BL    GRVERRPD                                                         
         DROP  R3                                                               
*                                                                               
GRV30    LTR   R0,R0               IF USING NEW GUARANTEE SYSTEM                
         BZ    GRVX                AND AGY/CLI MATCH NOT YET FOUND              
*                                                                               
         USING TAVAD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TAVAELQ      GET FIRST VALID AGENCY/CLIENT                
         BAS   RE,GETEL            ELEMENT                                      
         BNE   GRVERRAC            IF NOT FOUND, ERROR                          
*                                                                               
GRV40    CLC   TAVAAGY,TGAGY       IF AGENCY IS FOUND IN GRT LIMITS             
         BNE   GRV60                                                            
         CLI   TAVALEN,TAVALNQ     AND CLIENT LIMITS ARE NOT DEFINED            
         BE    GRVX                ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
GRV50    CLC   TGCLI,0(RF)         IF CLIENT IS FOUND IN GRT LIMITS             
         BE    GRVX                ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   GRV50                                                            
*                                                                               
GRV60    BAS   RE,NEXTEL           GET NEXT VALID AGENCY/CLIENT                 
         BE    GRV40               ELEMENT                                      
         B     GRVERRAC            IF NOT FOUND, RETURN ERROR                   
         DROP  R3                                                               
*                                                                               
GRVX     MVI   ERROR,0                                                          
         B     YES                                                              
*                                                                               
GRVERRAC MVI   TGGRTSTA,TGGRTEAC   RETURN AGENCY/CLIENT ERROR                   
         J     GRVERR                                                           
*                                                                               
GRVERRPD MVI   TGGRTSTA,TGGRTEPD   RETURN PERIOD ERROR                          
         J     GRVERR                                                           
*                                                                               
GRVERR   MVI   ERROR,TAINEGGU      RETURN ERROR                                 
         XC    0(4,R2),0(R2)       CLEAR A (TAGUELQ ELEMENT)                    
         B     NO                  AND SET CC NE                                
         EJECT                                                                  
EXPIRE   BRAS  RE,EXPIRET                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE A SINGLE DATE                                
*              MAXIMUM DATE INPUT STRING IS 12 CHARACTERS                       
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
*                                  P1=A(PWOS OUTPUT AREA)                       
*                                  P1 BYTE 0  X'80'=INPUT OPTIONAL              
*                                             X'40'=R2 IS A(DATA)               
DTVAL    DS    0H                                   WITH TRAILING SPACE         
         MVC   TGBYTE,0(R1)        SAVE HOBS                                    
         L     R3,0(R1)                                                         
         XC    0(3,R3),0(R3)       PRE-CLEAR OUTPUT AREA                        
         SPACE 1                                                                
         TM    TGBYTE,X'40'        IF NOT POINTING TO FIELD HEADER              
         BZ    DTVL3                                                            
         LR    R4,R2               R4=A(DATE)                                   
         LR    RE,R2               CALCULATE LENGTH OF DATE                     
         LA    RF,12                                                            
         XR    R1,R1                                                            
DTVL1    CLI   0(RE),C' '                                                       
         BE    DTVL2                                                            
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,DTVL1                                                         
DTVL2    STC   R1,TGBYTE2          TGBYTE2 IS L'DATE                            
         B     DTVL4                                                            
         SPACE 1                                                                
DTVL3    CLI   5(R2),0             IF NO INPUT                                  
         BNE   *+16                                                             
         TM    TGBYTE,X'80'        AND INPUT NOT OPTIONAL                       
         BZ    FLDMISS             THEN GIVE ERROR                              
         B     DTVLX               ELSE DONE                                    
         LA    R4,8(R2)            R4=A(DATE)                                   
         MVC   TGBYTE2,5(R2)       TGBYTE2 IS L'DATE                            
         SPACE 1                                                                
DTVL4    MVI   ACTUAL,5            RETURN L'DATE IN ACTUAL                      
         CLC   =C'TODAY',0(R4)     KEYWORD OF TODAY ALLOWED                     
         BNE   *+14                                                             
         MVC   0(3,R3),TGTODAY1                                                 
         B     DTVLX                                                            
         SPACE 1                                                                
         MVI   ACTUAL,8                                                         
         CLC   =C'NEXTBDAY',0(R4)  KEYWORD OF NEXT BUSINESS DAY                 
         BNE   *+14                                                             
         MVC   0(3,R3),TGNXTBUS                                                 
         B     DTVLX                                                            
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,(R4)),WORK VALIDATE FOR YMD                       
         MVC   ACTUAL,3(R1)                                                     
         CLI   ACTUAL,0                                                         
         BNE   DTVL6                                                            
         SPACE 1                                                                
         GOTO1 (RF),(R1),(1,(R4)),WORK   ELSE VALIDATE FOR MD                   
         MVC   ACTUAL,3(R1)                                                     
         CLI   ACTUAL,0                                                         
         BE    DTVLERR                                                          
         SPACE 1                                                                
         MVC   WORK(2),TGTODAY0          OK, SO SET THIS YEAR                   
         CLC   WORK+2(4),=C'0229'  IF 2/29 (DATVAL ALLOWS IT W/O YEAR)          
         BNE   DTVL6                                                            
         CLC   TGBYTE2,ACTUAL      ERROR IF VALID DATE LENGTH <> INPUT          
         BNE   DTVLERR                                                          
         MVC   DUB(4),WORK+2       INPUT IS 2/29 W/O YEAR                       
         MVC   DUB+4(2),TGTODY20+2 USE DISPLAYABLE YEAR                         
         GOTO1 (RF),(R1),(0,DUB),WORK    RE-VALIDATE FOR THIS YEAR              
         CLI   3(R1),0                                                          
         BE    DTVLERR                                                          
         SPACE 1                                                                
DTVL6    GOTO1 DATCON,DMCB,(0,WORK),(1,(R3))  RETURN PWOS DATE                  
         SPACE 1                                                                
* 2/95   CLI   0(R3),X'98'         DON'T ALLOW DATES ON/AFTER 1/1/98            
* NO-OP  BNL   DTVLERR                                                          
         SPACE 1                                                                
DTVLX    B     YES                 RETURN CC EQ                                 
         SPACE 2                                                                
DTVLERR  TM    TGBYTE,X'40'        IF POINTING TO FIELD HEADER                  
         BZ    DATINV              GIVE ERROR                                   
         B     NO                  ELSE RETURN CC NE                            
         EJECT                                                                  
*              ROUTINE TO PRINT LINES DISPLAYED ON SCREEN                       
*                                  P1=A(START)                                  
*                                  P2=A(END) OR 0 FOR 1 LINE                    
*                                  P3=A(OUTPUT AREA)                            
*                                  BYTE='P'RINT OR 'H'EADS INDICATOR            
PRTSCRN  DS    0H                                                               
         LM    R2,R4,0(R1)         R2=A(START), R3=A(END), R4=A(OUTPUT)         
         XR    R0,R0               R0=STARTING ROW NUMBER                       
         SPACE 1                                                                
PRT10    CLI   1(R2),X'FF'         IGNORE NOP FIELDS                            
         BE    PRT30                                                            
         LH    RF,2(R2)            CALCULATE ROW AND COLUMN                     
         XR    RE,RE                                                            
         D     RE,=F'80'           RE=COLUMN-1 OF CURRENT FIELD                 
         SPACE 1                                                                
         LA    R5,0(RE,R4)         R5=A(CORRES. POSITION IN PRINT LINE)         
         SPACE 1                                                                
         LTR   R0,R0               IF THIS IS FIRST TIME THROUGH                
         BNZ   *+6                                                              
         LR    R0,RF               SAVE STARTING ROW NUMBER (-1)                
         SPACE 1                                                                
         SR    RF,R0               IF ROW HAS CHANGED                           
         BNP   PRT20                                                            
         LTR   R3,R3               THEN IF ONLY WANT ONE LINE                   
         BZ    PRTX                THEN DONE                                    
         AR    R0,RF               ELSE SAVE NEW ROW NUMBER (-1)                
         SPACE 1                                                                
         CLI   BYTE,C'P'           IF PRINTING                                  
         BNE   *+16                                                             
         STC   RF,SPACING          SET SPACING BASED ON N'LINES SKIPPED         
         BAS   RE,PRNTIT           AND PRINT CURRENT LINE                       
         B     PRT20                                                            
         MHI   RF,132              ELSE SET TO BUMP HEADINGS                    
         AR    R4,RF               BUMP TO NEXT HEADING FIELD                   
         AR    R5,RF               BUMP CURRENT POSITION                        
         SPACE 1                                                                
PRT20    ZIC   RE,0(R2)            SET RE=L'FIELD DATA                          
         AHI   RE,-9                                                            
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),8(R2)       MOVE SCREEN FIELD TO PRINT LINE              
         SPACE 1                                                                
PRT30    BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         LTR   R3,R3               IF END IS DEFINED                            
         BZ    PRT10                                                            
         CR    R2,R3               THEN AS LONG AS HAVEN'T REACHED IT           
         BL    PRT10               CONTINUE                                     
         SPACE 1                                                                
PRTX     CLI   BYTE,C'P'           IF PRINTING                                  
         BNE   *+8                                                              
         BAS   RE,PRNTIT           PRINT LAST LINE                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE UPDATES NEXT INVOICE NUM IN AGENCY RECORD IN AIO         
*              AND SETS TGINV TO THE FIRST INVOICE NUMBER TO ADD                
*              SETS CC AND RETURNS NEXT INVOICE NUMBER FOR AGENCY               
*              IN PLIST AND PACKED NUMBER OF FIRST INVOICE IN PLIST+6           
*              AND FILLS IN DISPLAYABLE FIRST INVOICE NUMBER IN                 
*              ADDRESS PROVIDED                                                 
         SPACE                                                                  
*                                P1=A(PACKED NUMBER OF INVOICES TO ADD)         
*                                P2=A(DISPLAYABLE FIRST INVOICE NUMBER)         
*                                P4=X'FF' - RESET AGY HAS LOWER INV #           
         SPACE                                                                  
         USING TAAYD,R3                                                         
CHNINV   DS    0H                                                               
         L     R2,0(R1)                                                         
         MVC   HALF,0(R2)                                                       
         L     R4,4(R1)                                                         
         LR    R2,R1               SAVE A(PLIST) IN R2                          
         MVI   BYTE,0                                                           
         SPACE                                                                  
         MVC   ELEMENT(L'TGAGY),TGAGY  SAVE ORIGINAL AGENCY                     
         L     R3,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    TAAYIAGY,TAAYIAGY   IF USING INV NUMBERS OF ANOTHER AGY          
         BZ    CHN8                                                             
         MVI   BYTE,C'Y'           SET FLAG                                     
         SPACE                                                                  
         GOTO1 RECVALL,DMCB,(X'80',TLAYCDQ),(X'B0',TAAYIAGY)  UPD REC           
         BNE   NO                  IF NOT FOUND RETURN CC NOT EQUAL             
         SPACE                                                                  
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    TAAYIAGY,TAAYIAGY   IF USING INV NUMBERS OF ANOTHER AGY          
         BNZ   NO                  RETURN CC NOT EQUAL                          
         SPACE                                                                  
         USING TAAYD,R3                                                         
CHN8     PACK  FULL(3),TGTODY20(5)      GET CCYY                                
         CLC   TAAYNINV(2),FULL         IF CCYY                                 
         BNE   *+14                                                             
         CLC   TAAYNINV+2(1),TGTODAY1+1 OR MONTH DOESN'T MATCH                  
         BE    CHN10                                                            
         MVC   TAAYNINV(2),FULL         RESET WITH TODAY'S CENTURY & YR         
         MVC   TAAYNINV+2(1),TGTODAY1+1                    MONTH                
         MVC   TAAYNINV+3(2),TAAYRINV         & USE RESET INVOICE NUMB.         
         SPACE                                                                  
CHN10    CLI   BYTE,C'Y'           USE INV FROM ANOTHER AGY?                    
         BNE   CHN14                                                            
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING TLIND,RF                                                         
         MVI   TLINCD,TLINCDQ      CHECK IF INVOICE NUMBER EXISTS FOR           
         MVC   TLINAGY,ELEMENT     SAVED ORIGINAL AGENCY                        
         MVC   TLININV,TAAYNINV                                                 
         XC    TLININV,=6X'FF'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLINLEN-TLINCD),KEYSAVE                                      
         BNE   CHN12                                                            
         MVI   12(R2),X'FF'        PASS APPLICATION ERROR STATUS                
         B     NO                                                               
         SPACE                                                                  
CHN12    XC    KEY,KEY             RESET READ SEQUENCE                          
         L     RF,AIO                                                           
         MVC   KEY(L'TLDRKEY),0(RF)                                             
         GOTO1 HIGH                                                             
         DROP  RF                                                               
         SPACE                                                                  
CHN14    MVI   TAAYNINV+5,0             ALWAYS ADD NEW TP TYPE INV NUM          
         MVC   TGINV,TAAYNINV           SAVE FIRST INVOICE NUMBER               
         SPACE                                                                  
         GOTO1 =V(TINVCON),DMCB,TGINV,(R4),DATCON,RR=TGRELOV                    
         MVI   FULL+2,X'0C'             INIT TO 0 PACKED                        
         MVO   FULL(3),TGINV+3(2)       SAVE PACKED NUMBER OF INVOICE           
         XC    TGINV,=6X'FF'            COMPLEMENT GLOBAL INVOICE               
         MVC   TGFULL(3),FULL                                                   
         AP    TGFULL(3),HALF      UPDATE NEXT INVOICE NUMBER                   
         MVO   WORK(3),TGFULL(3)                                                
         MVC   TAAYNINV+3(2),WORK  MOVE TO RECORD-ALL ELSE THE SAME             
         MVC   TGDUB(6),TAAYNINV   SAVE NEXT INVOICE NUMBER                     
         GOTO1 PUTREC              WRITE BACK CHANGE                            
         SPACE                                                                  
         CLI   BYTE,C'Y'           IF SHARING INVOICE NUMBERS                   
         BNE   CHNX                                                             
         GOTO1 RECVALL,DMCB,TLAYCDQ,(X'B0',ELEMENT)  UPD ORIG AGY REC           
         SPACE                                                                  
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TAAYNINV,TGDUB      UPDATE NEXT INVOICE NUMBER                   
         GOTO1 PUTREC              WRITE BACK CHANGE                            
         SPACE                                                                  
CHNX     MVC   0(6,R2),TGDUB       RETURN NEXT INVOICE NUMBER AND               
         MVC   6(3,R2),FULL        PACKED NUMBER OF INVOICE IN PLIST            
         B     YES                 RETURN CC EQUAL                              
         EJECT                                                                  
*              ROUTINE TO GET THRESHOLD FOR AN AIRDATE                          
*                                                                               
*                                  P1 = A(3-BYTE AIR DATE)                      
*                                  P1 BYTE 0  X'80'=PASS 30M THRESHOLD          
*                                  TGFULL = THRESHOLD RETURNED                  
         SPACE 1                                                                
GETTHRES DS    0H                                                               
         L     R2,0(R1)                     R2 = A(3 BYTE AIR DATE)             
         CLC   0(L'THRESEDT,R2),=X'950301'  IF AIR DT NOT BEFORE 3/1/95         
         BNL   NO                           RETURN NEQ - NO THRESHOLD           
*                                                                               
         L     R3,TGATHRES         R3 = A(THRESHOLD TABLE)                      
         USING THRESTBD,R3                                                      
*                                                                               
GETTH10  CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         BE    NO                                                               
*                                                                               
         CLC   0(L'THRESEDT,R2),THRESEDT  IF AIR DT NOT BEFORE END DT           
         BNH   GETTH20                                                          
         LA    R3,THRESTBL(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     GETTH10             LOOP                                         
*                                                                               
GETTH20  TM    0(R1),X'80'         IF REQUESTING 30M THRESHOLD                  
         BNO   *+14                                                             
         MVC   TGFULL,THRES30M     GIVE IT                                      
         B     *+10                                                             
         MVC   TGFULL,THRES1HR     ELSE GIVE 1HR THRESHOLD                      
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE UPDATES NEXT ADVICE NUMBER FOR AN AGENCY                 
*              SETS CC AND TGADV TO THE NEXT ADVICE NUMBER                      
*                                                                               
         SPACE 1                                                                
CHNADV   DS    0H                                                               
         GOTO1 RECVALL,DMCB,(X'80',TLAYCDQ),(X'B0',TGAGY)  UPD REC              
         BNE   NO                  IF NOT FOUND RETURN CC NOT EQUAL             
         SPACE                                                                  
CHNADV00 MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R3                                                         
*                                                                               
         OC    TAAYAAGY,TAAYAAGY   IF PARENT AGENCY DEFINED                     
         BZ    CHNADV05            USE THAT AGENCY'S NEXT ADV#                  
         MVC   TGDUB(L'TGAGY),TGAGY                                             
         GOTO1 RECVALL,DMCB,(X'80',TLAYCDQ),(X'B0',TAAYAAGY)                    
         MVC   TGAGY,TGDUB                                                      
         BNE   NO                                                               
         B     CHNADV00                                                         
*                                                                               
CHNADV05 GOTO1 DATCON,DMCB,(1,TGTODAY1),(3,TGFULL)                              
         ZIC   R1,TGFULL+1           BINARY MONTH                               
         BCTR  R1,0                                                             
         LA    RE,LETTAB                                                        
         AR    RE,R1                                                            
         MVC   TGBYTE,0(RE)          SET LETTER CORRES TO MONTH (A-L)           
         MVC   TGBYTE2,TGTODAY0+1    LAST DIGIT OF YEAR (0-9)                   
*                                                                               
         CLC   TGBYTE(2),TAAYNADV    IF MONTH OR YEAR DOESN'T MATCH             
         BE    CHNADV10                                                         
         MVC   TAAYNADV(2),TGBYTE    RESET ADVICE CODE                          
         GOTO1 HEXOUT,DMCB,TAAYRADV,TAAYNADV+2,2,0                              
*        MVC   TAAYNADV+2(4),=C'0000'                                           
*                                                                               
CHNADV10 MVC   TGADV,TAAYNADV        SET ADVICE CODE                            
*                                                                               
         BRAS  RE,UPDADV                                                        
         GOTO1 PUTREC              WRITE BACK UPDATED AGENCY RECORD             
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 ,DMCB,(R8)          SET PARMS FOR SPOOL                          
         L     RF,SPOOL            SET RF=A(SPOOL)                              
         L     RE,TGRD                                                          
         L     RE,4(RE)                                                         
         LM    R2,RC,28(RE)        RESTORE CALLER'S REGS (FOR HEADHOOK)         
         BASR  RE,RF               OFF TO SPOOL                                 
         XIT1                      STILL HAVE CALLER'S REGS - USE MACRO         
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
RECNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
VIOLATER MVI   ERROR,ERACCESS      ACCESS ERROR - TOO MANY TRIES                
         B     SERVEND                                                          
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
DATINV   MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
         USING TLDRD,R5                                                         
RACTERR  MVI   ERROR,ERSECR        SECURITY RECORD ERROR WITH RECACT            
         MVI   BYTE,GTMERR         SET ERROR TYPE FOR GETTXT                    
         MVI   BLOCK,9                                                          
         GOTO1 HEXOUT,DMCB,TLDRDA,BLOCK+1,4,0                                   
         MVI   BLOCK+9,0                                                        
         B     TEXTEND                                                          
         SPACE 1                                                                
TEXTEND  XC    GETTXTCB,GETTXTCB   DEFINE CONTROL BLOCK                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMTYP,BYTE         SET MESSAGE TYPE                             
         MVC   GTMSGNO+1(1),ERROR  AND MESSAGE NUMBER                           
         MVC   GTMSYS,GETMSYS      AND MESSAGE SYSTEM                           
         LA    RE,BLOCK            IN CASE IT'S DEFINED                         
         STCM  RE,7,GTASUBST       SET A(SUBSTITUTION BLOCK)                    
         OI    GENSTAT2,USGETTXT   LET GENCON KNOW WE'VE SET GETTXTCB           
SERVEND  LA    R2,CONSERVH         CURSOR TO SERVICE REQUEST FIELD              
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
ZEROS    DC    16X'00'                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              LETTER TABLE OF MONTHS                                           
         SPACE 1                                                                
LETTAB   DS    0CL12                                                            
         DC    C'ABCDEFGHIJKL'                                                  
         EJECT                                                                  
*              TABLE USED BY RECVAL TO BUILD KEYS                               
         SPACE 1                                                                
RECTAB   DS    0C                                                               
         DC    AL1(RSYX-*,TLSYCDQ,0)                      SYSTEM                
RSYX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RSTNX-*,TLSTCDQ,(RSTNX-RSTN)/FLDLNQ)   STAFF                 
RSTN     DC    AL1(TLSTSTAF-TLSTD),AL2(TGSTAF-TGD),AL1(L'TLSTSTAF-1)            
RSTNX    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RSTX-*,TLSTCDQ,(RSTX-RST)/FLDLNQ)      STAFF                 
RST      DC    AL1(TLSTUSER-TLSTD),AL2(TGUSER-TGD),AL1(L'TLSTUSER-1)            
         DC    AL1(TLSTSTAF-TLSTD),AL2(TGSTAF-TGD),AL1(L'TLSTSTAF-1)            
RSTX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(ROFX-*,TLOFCDQ,(ROFX-ROF)/FLDLNQ)      OFFICE                
ROF      DC    AL1(TLOFOFF-TLOFD),AL2(TGOFF-TGD),AL1(L'TLOFOFF-1)               
ROFX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RAYX-*,TLAYCDQ,(RAYX-RAY)/FLDLNQ)      AGENCY                
RAY      DC    AL1(TLAYAGY-TLAYD),AL2(TGAGY-TGD),AL1(L'TLAYAGY-1)               
RAYX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RAGX-*,TLAGCDQ,(RAGX-RAG)/FLDLNQ)      AGENCY GROUP          
RAG      DC    AL1(TLAGAGG-TLAGD),AL2(TGAGG-TGD),AL1(L'TLAGAGG-1)               
RAGX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RATX-*,TLATCDQ,(RATX-RAT)/FLDLNQ)      ATTENTION             
RAT      DC    AL1(TLATAGY-TLATD),AL2(TGAGY-TGD),AL1(L'TLATAGY-1)               
         DC    AL1(TLATATT-TLATD),AL2(TGATT-TGD),AL1(L'TLATATT-1)               
RATX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCLX-*,TLCLCDQ,(RCLX-RCL)/FLDLNQ)      CLIENT (AGY)          
RCL      DC    AL1(TLCLAGY-TLCLD),AL2(TGAGY-TGD),AL1(L'TLCLAGY-1)               
         DC    AL1(TLCLCLI-TLCLD),AL2(TGCLI-TGD),AL1(L'TLCLCLI-1)               
RCLX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCL2X-*,TLCLCDQ,(RCL2X-RCL2)/FLDLNQ)   CLIENT                
RCL2     DC    AL1(TLCLCLI-TLCLD),AL2(TGCLI-TGD),AL1(L'TLCLCLI-1)               
RCL2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCGX-*,TLCGCDQ,(RCGX-RCG)/FLDLNQ)      CLIENT GROUP          
RCG      DC    AL1(TLCGCLG-TLCGD),AL2(TGCLG-TGD),AL1(L'TLCGCLG-1)               
RCGX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RPRX-*,TLPRCDQ,(RPRX-RPR)/FLDLNQ)      PRODUCT (AGY)         
RPR      DC    AL1(TLPRAGY-TLPRD),AL2(TGAGY-TGD),AL1(L'TLPRAGY-1)               
         DC    AL1(TLPRCLI-TLPRD),AL2(TGCLI-TGD),AL1(L'TLPRCLI-1)               
         DC    AL1(TLPRPRD-TLPRD),AL2(TGPRD-TGD),AL1(L'TLPRPRD-1)               
RPRX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RPR2X-*,TLPRCDQ,(RPR2X-RPR2)/FLDLNQ)   PRODUCT               
RPR2     DC    AL1(TLPRPRD-TLPRD),AL2(TGPRD-TGD),AL1(L'TLPRPRD-1)               
RPR2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RPGX-*,TLPGCDQ,(RPGX-RPG)/FLDLNQ)      PRODUCT GROUP         
RPG      DC    AL1(TLPGAGY-TLPGD),AL2(TGAGY-TGD),AL1(L'TLPGAGY-1)               
         DC    AL1(TLPGCLI-TLPGD),AL2(TGCLI-TGD),AL1(L'TLPGCLI-1)               
         DC    AL1(TLPGPRG-TLPGD),AL2(TGPRG-TGD),AL1(L'TLPGPRG-1)               
RPGX     EQU   *                                                                
         SPACE 1                                        COMMERCIAL              
         DC    AL1(RCOX-*,TLCOICDQ,(RCOX-RCO)/FLDLNQ)   (COMM ID)               
RCO      DC    AL1(TLCOIAGY-TLCOPD),AL2(TGAGY-TGD),AL1(L'TLCOIAGY-1)            
         DC    AL1(TLCOICID-TLCOPD),AL2(TGCID-TGD),AL1(L'TLCOICID-1)            
RCOX     EQU   *                                                                
         SPACE 1                                        COMMERCIAL              
         DC    AL1(RCO2X-*,TLCOCDQ,(RCO2X-RCO2)/FLDLNQ) (AGY/CLI/PRD)           
RCO2     DC    AL1(TLCOAGY-TLCOD),AL2(TGAGY-TGD),AL1(L'TLCOAGY-1)               
         DC    AL1(TLCOCLI-TLCOD),AL2(TGCLI-TGD),AL1(L'TLCOCLI-1)               
         DC    AL1(TLCOPRD-TLCOD),AL2(TGPRD-TGD),AL1(L'TLCOPRD-1)               
         DC    AL1(TLCOCID-TLCOD),AL2(TGCID-TGD),AL1(L'TLCOCID-1)               
         DC    AL1(TLCOCOM-TLCOD),AL2(TGCOM-TGD),AL1(L'TLCOCOM-1)               
RCO2X    EQU   *                                                                
         SPACE 1                                          COMMERCIAL            
         DC    AL1(RCO3X-*,TLCOCCDQ,(RCO3X-RCO3)/FLDLNQ)  (INTERNAL #)          
RCO3     DC    AL1(TLCOCCOM-TLCOPD),AL2(TGCOM-TGD),AL1(L'TLCOCCOM-1)            
RCO3X    EQU   *                                                                
         SPACE 1                                          COMMERCIAL            
         DC    AL1(RCO4X-*,TLCOGCDQ,(RCO4X-RCO4)/FLDLNQ)  (CLI GRP)             
RCO4     DC    AL1(TLCOGCLG-TLCOPD),AL2(TGCLG-TGD),AL1(L'TLCOGCLG-1)            
         DC    AL1(TLCOGCID-TLCOPD),AL2(TGCID-TGD),AL1(L'TLCOGCID-1)            
RCO4X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RMUX-*,TLMUCDQ,(RMUX-RMU)/FLDLNQ)      MUSIC                 
RMU      DC    AL1(TLMUMUS-TLMUD),AL2(TGMUS-TGD),AL1(L'TLMUMUS-1)               
RMUX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RANX-*,TLANCDQ,(RANX-RAN)/FLDLNQ)      AGENT                 
RAN      DC    AL1(TLANAGT-TLAND),AL2(TGAGT-TGD),AL1(L'TLANAGT-1)               
RANX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RAN2X-*,TLANCCDQ,(RAN2X-RAN2)/FLDLNQ)  AGENT NUM             
RAN2     DC    AL1(TLANCAGT-TLANPD),AL2(TGAGT-TGD),AL1(L'TLANCAGT-1)            
RAN2X    EQU   *                                                                
         SPACE 1                                          INTERFACE             
         DC    AL1(RIFX-*,TLIFCDQ,(RIFX-RIF)/FLDLNQ)      (CLIENT)              
RIF      DC    AL1(TLIFCLI-TLIFD),AL2(TGPCLI-TGD),AL1(L'TLIFPCLI-1)             
         DC    AL1(TLIFAGY-TLIFD),AL2(TGAGY-TGD),AL1(L'TLIFAGY-1)               
RIFX     EQU   *                                                                
         SPACE 1                                          INTERFACE             
         DC    AL1(RIF2X-*,TLIFCDQ,(RIF2X-RIF2)/FLDLNQ)   (AGENCY)              
RIF2     DC    AL1(TLIFAGY-TLIFD),AL2(TGAGY-TGD),AL1(L'TLIFAGY-1)               
RIF2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(REMX-*,TLEMCDQ,(REMX-REM)/FLDLNQ)      EMPLOYER              
REM      DC    AL1(TLEMEMP-TLEMD),AL2(TGEMP-TGD),AL1(L'TLEMEMP-1)               
REMX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(REXX-*,TLEXCDQ,(REXX-REX)/FLDLNQ)      EMP TAX               
REX      DC    AL1(TLEXEMP-TLEXD),AL2(TGEMP-TGD),AL1(L'TLEXEMP-1)               
REXX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RW4X-*,TLW4CDQ,(RW4X-RW4)/FLDLNQ)      W4                    
RW4      DC    AL1(TLW4SSN-TLW4D),AL2(TGSSN-TGD),AL1(L'TLW4SSN-1)               
RW4X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RGUX-*,TLGUCDQ,(RGUX-RGU)/FLDLNQ)      GUARANTEE             
RGU      DC    AL1(TLGUSSN-TLGUD),AL2(TGSSN-TGD),AL1(L'TLGUSSN-1)               
         DC    AL1(TLGUGUA-TLGUD),AL2(TGGUA-TGD),AL1(L'TLGUGUA-1)               
RGUX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RGCX-*,TLGCCDQ,(RGCX-RGC)/FLDLNQ)      GRT CONTRACT          
RGC      DC    AL1(TLGCSSN-TLGCD),AL2(TGSSN-TGD),AL1(L'TLGCSSN-1)               
         DC    AL1(TLGCGCNT-TLGCD),AL2(TGGCNT-TGD),AL1(L'TLGCGCNT-1)            
RGCX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RDUX-*,TLDUCDQ,(RDUX-RDU)/FLDLNQ)      DUE COMPANY           
RDU      DC    AL1(TLDUSSN-TLDUD),AL2(TGSSN-TGD),AL1(L'TLDUSSN-1)               
         DC    AL1(TLDUDUC-TLDUD),AL2(TGDUC-TGD),AL1(L'TLDUDUC-1)               
RDUX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RLNX-*,TLLNCDQ,(RLNX-RLN)/FLDLNQ)      LIEN                  
RLN      DC    AL1(TLLNSSN-TLLND),AL2(TGSSN-TGD),AL1(L'TLLNSSN-1)               
         DC    AL1(TLLNLIN-TLLND),AL2(TGLIN-TGD),AL1(L'TLLNLIN-1)               
RLNX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCAX-*,TLCACDQ,(RCAX-RCA)/FLDLNQ)      CAST (ACTIVE)         
RCA      DC    AL1(TLCACOM-TLCAD),AL2(TGCOM-TGD),AL1(L'TLCACOM-1)               
         DC    AL1(TLCASORT-TLCAD),AL2(TGCSORT-TGD),AL1(L'TLCASORT-1)           
         DC    AL1(TLCASSN-TLCAD),AL2(TGSSN-TGD),AL1(L'TLCASSN-1)               
         DC    AL1(TLCACAT-TLCAD),AL2(TGCAT-TGD),AL1(L'TLCACAT-1)               
RCAX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCA2X-*,TLCACCDQ,(RCA2X-RCA2)/FLDLNQ)  CAST (COMMLS)         
RCA2     DC    AL1(TLCACSSN-TLCAPD),AL2(TGSSN-TGD),AL1(L'TLCACSSN-1)            
         DC    AL1(TLCACCOM-TLCAPD),AL2(TGCOM-TGD),AL1(L'TLCACCOM-1)            
         DC    AL1(TLCACCAT-TLCAPD),AL2(TGCAT-TGD),AL1(L'TLCACCAT-1)            
         DC    AL1(TLCACSEQ-TLCAPD),AL2(TGCSORT+4-TGD)                          
         DC    AL1(L'TLCACSEQ-1)                                                
RCA2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCA3X-*,TLCAGCDQ,(RCA3X-RCA3)/FLDLNQ)  CAST (GUARS)          
RCA3     DC    AL1(TLCAGSSN-TLCAPD),AL2(TGSSN-TGD),AL1(L'TLCAGSSN-1)            
         DC    AL1(TLCAGGUA-TLCAPD),AL2(TGGUA-TGD),AL1(L'TLCAGGUA-1)            
         DC    AL1(TLCAGCOM-TLCAPD),AL2(TGCOM-TGD),AL1(L'TLCAGCOM-1)            
         DC    AL1(TLCAGCAT-TLCAPD),AL2(TGCAT-TGD),AL1(L'TLCAGCAT-1)            
         DC    AL1(TLCAGSEQ-TLCAPD),AL2(TGCSORT+4-TGD)                          
         DC    AL1(L'TLCAGSEQ-1)                                                
RCA3X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RARX-*,TLARCDQ,(RARX-RAR)/FLDLNQ)       AREA                 
RAR      DC    AL1(TLARAREA-TLARD),AL2(TGAREA-TGD),AL1(L'TLARAREA-1)            
RARX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RUSX-*,TLUSCDQ,(RUSX-RUS)/FLDLNQ)       USE                  
RUS      DC    AL1(TLUSUSE-TLUSD),AL2(TGUSE-TGD),AL1(L'TLUSUSE-1)               
RUSX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RUHX-*,TLUHCDQ,(RUHX-RUH)/FLDLNQ)       USAGE HIST.          
RUH      DC    AL1(TLUHCOM-TLUHD),AL2(TGCOM-TGD),AL1(L'TLUHCOM-1)               
         DC    AL1(TLUHCSEQ-TLUHD),AL2(TGCSORT+4-TGD)                           
         DC    AL1(L'TLUHCSEQ-1)                                                
         DC    AL1(TLUHUSE-TLUHD),AL2(TGUSCDE-TGD),AL1(L'TLUHUSE-1)             
         DC    AL1(TLUHINV-TLUHD),AL2(TGINV-TGD),AL1(L'TLUHINV-1)               
RUHX     EQU   *                                                                
         SPACE 1                                         DEAL                   
         DC    AL1(RDLX-*,TLDLCDQ,(RDLX-RDL)/FLDLNQ)     (CLIENT)               
RDL      DC    AL1(TLDLCLI-TLDLD),AL2(TGCLI-TGD),AL1(L'TLDLCLI-1)               
         DC    AL1(TLDLAGY-TLDLD),AL2(TGAGY-TGD),AL1(L'TLDLAGY-1)               
RDLX     EQU   *                                                                
         SPACE 1                                         DEAL                   
         DC    AL1(RDL2X-*,TLDLCDQ,(RDL2X-RDL2)/FLDLNQ)  (AGENCY)               
RDL2     DC    AL1(TLDLAGY-TLDLD),AL2(TGAGY-TGD),AL1(L'TLDLAGY-1)               
RDL2X    EQU   *                                                                
         SPACE 1                                         INVOICE                
         DC    AL1(RINX-*,TLINCDQ,(RINX-RIN)/FLDLNQ)     (ACCESS)               
RIN      DC    AL1(TLINAGY-TLIND),AL2(TGAGY-TGD),AL1(L'TLINAGY-1)               
         DC    AL1(TLININV-TLIND),AL2(TGINV-TGD),AL1(L'TLININV-1)               
RINX     EQU   *                                                                
         SPACE 1                                          INVOICE               
         DC    AL1(RIN2X-*,TLINBCDQ,(RIN2X-RIN2)/FLDLNQ)  (STATUS)              
RIN2     DC    AL1(TLINBAGY-TLINPD),AL2(TGAGY-TGD),AL1(L'TLINBAGY-1)            
         DC    AL1(TLINBINV-TLINPD),AL2(TGINV-TGD),AL1(L'TLINBINV-1)            
RIN2X    EQU   *                                                                
         SPACE 1                                           INVOICE              
         DC    AL1(RIN3X-*,TLINHCDQ,(RIN3X-RIN3)/FLDLNQ)   (PYMT HIST)          
RIN3     DC    AL1(TLINHCOM-TLINPD),AL2(TGCOM-TGD),AL1(L'TLINHCOM-1)            
         DC    AL1(TLINHINV-TLINPD),AL2(TGINV-TGD),AL1(L'TLINHINV-1)            
RIN3X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCKX-*,TLCKCDQ,(RCKX-RCK)/FLDLNQ)     CHECK (ACTIVE)         
RCK      DC    AL1(TLCKAGY-TLCKD),AL2(TGAGY-TGD),AL1(L'TLCKAGY-1)               
         DC    AL1(TLCKINV-TLCKD),AL2(TGINV-TGD),AL1(L'TLCKINV-1)               
         DC    AL1(TLCKSORT-TLCKD),AL2(TGCSORT-TGD),AL1(L'TLCKSORT-1)           
         DC    AL1(TLCKSSN-TLCKD),AL2(TGSSN-TGD),AL1(L'TLCKSSN-1)               
         DC    AL1(TLCKCAT-TLCKD),AL2(TGCAT-TGD),AL1(L'TLCKCAT-1)               
RCKX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCK2X-*,TLCKCCDQ,(RCK2X-RCK2)/FLDLNQ) CHECK (CHECKS)         
RCK2     DC    AL1(TLCKCBNK-TLCKPD),AL2(TGBNK-TGD),AL1(L'TLCKCBNK-1)            
         DC    AL1(TLCKCCHK-TLCKPD),AL2(TGCHK-TGD),AL1(L'TLCKCCHK-1)            
RCK2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RESX-*,TLESCDQ,(RESX-RES)/FLDLNQ)      ESTIMATE              
RES      DC    AL1(TLESAGY-TLESD),AL2(TGAGY-TGD),AL1(L'TLESAGY-1)               
         DC    AL1(TLESEST-TLESD),AL2(TGEST-TGD),AL1(L'TLESEST-1)               
RESX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RLOX-*,TLLOCDQ,(RLOX-RLO)/FLDLNQ)      LOCAL                 
RLO      DC    AL1(TLLOUN-TLLOD),AL2(TGUNI-TGD),AL1(L'TLLOUN-1)                 
         DC    AL1(TLLOLCL-TLLOD),AL2(TGLCL-TGD),AL1(L'TLLOLCL-1)               
RLOX     EQU   *                                                                
         SPACE 1                                          CONTROL               
         DC    AL1(RCTX-*,TLCTCDQ,(RCTX-RCT)/FLDLNQ)      (CLIENT)              
RCT      DC    AL1(TLCTCLI-TLCTD),AL2(TGCLI-TGD),AL1(L'TLCTCLI-1)               
         DC    AL1(TLCTAGY-TLCTD),AL2(TGAGY-TGD),AL1(L'TLCTAGY-1)               
RCTX     EQU   *                                                                
         SPACE 1                                          CONTROL               
         DC    AL1(RCT2X-*,TLCTCDQ,(RCT2X-RCT2)/FLDLNQ)   (AGENCY)              
RCT2     DC    AL1(TLCTAGY-TLCTD),AL2(TGAGY-TGD),AL1(L'TLCTAGY-1)               
RCT2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RBAX-*,TLBACDQ,(RBAX-RBA)/FLDLNQ)      BALANCE               
RBA      DC    AL1(TLBADATE-TLBAD),AL2(TGDDTE-TGD),AL1(L'TLBADATE-1)            
         DC    AL1(TLBACURR-TLBAD),AL2(TGTYCUR-TGD),AL1(L'TLBACURR-1)           
         DC    AL1(TLBAEMP-TLBAD),AL2(TGEMP-TGD),AL1(L'TLBAEMP-1)               
RBAX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RDVX-*,TLDVCDQ,(RDVX-RDV)/FLDLNQ)       ADVICE               
RDV      DC    AL1(TLDVAGY-TLDVD),AL2(TGAGY-TGD),AL1(L'TLDVAGY-1)               
         DC    AL1(TLDVCID-TLDVD),AL2(TGCID-TGD),AL1(L'TLDVCID-1)               
         DC    AL1(TLDVADV-TLDVD),AL2(TGADV-TGD),AL1(L'TLDVADV-1)               
RDVX     EQU   *                                                                
         SPACE 1                                           ADVICE               
         DC    AL1(RDCX-*,TLDCCDQ,(RDCX-RDC)/FLDLNQ)       CAST REC             
RDC      DC    AL1(TLDCAGY-TLDCD),AL2(TGAGY-TGD),AL1(L'TLDCAGY-1)               
         DC    AL1(TLDCCID-TLDCD),AL2(TGCID-TGD),AL1(L'TLDCCID-1)               
         DC    AL1(TLDCADV-TLDCD),AL2(TGADV-TGD),AL1(L'TLDCADV-1)               
RDCX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RNXX-*,TLNXCDQ,(RNXX-RNX)/FLDLNQ)       NETWORK XFER         
RNX      DC    AL1(TLNXAGY-TLNXD),AL2(TGAGY-TGD),AL1(L'TLNXAGY-1)               
         DC    AL1(TLNXNID-TLNXD),AL2(TGNID-TGD),AL1(L'TLNXNID-1)               
         DC    AL1(TLNXNCLI-TLNXD),AL2(TGNCLI-TGD),AL1(L'TLNXNCLI-1)            
         DC    AL1(TLNXNPRD-TLNXD),AL2(TGNPRD-TGD),AL1(L'TLNXNPRD-1)            
         DC    AL1(TLNXMED-TLNXD),AL2(TGMENAME-TGD),AL1(L'TLNXMED-1)            
         DC    AL1(TLNXUSE-TLNXD),AL2(TGUSCDE-TGD),AL1(L'TGUSCDE-1)             
         DC    AL1(TLNXDATE-TLNXD),AL2(TGDATE-TGD),AL1(L'TLNXDATE-1)            
         DC    AL1(TLNXUID-TLNXD),AL2(TGUSER-TGD),AL1(L'TLNXUID-1)              
         DC    AL1(TLNXCCDE-TLNXD),AL2(TGTYPE-TGD),AL1(L'TLNXCCDE-1)            
RNXX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RAKX-*,TLAKCDQ,(RAKX-RAK)/FLDLNQ)       ALIAS                
RAK      DC    AL1(TLAKAGY-TLAKD),AL2(TGAGY-TGD),AL1(L'TLAKAGY-1)               
         DC    AL1(TLAKADID-TLAKD),AL2(TGADID-TGD),AL1(L'TLAKADID-1)            
         DC    AL1(TLAKNCLI-TLAKD),AL2(TGNCLI-TGD),AL1(L'TLAKNCLI-1)            
         DC    AL1(TLAKNPRD-TLAKD),AL2(TGNPRD-TGD),AL1(L'TLAKNPRD-1)            
         DC    AL1(TLAKMED-TLAKD),AL2(TGMENAME-TGD),AL1(L'TLAKMED-1)            
RAKX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RJBX-*,TLJBCDQ,(RJBX-RJB)/FLDLNQ)       JOB                  
RJB      DC    AL1(TLJBAGY-TLJBD),AL2(TGAGY-TGD),AL1(L'TLJBAGY-1)               
         DC    AL1(TLJBDTE-TLJBD),AL2(TGDATE-TGD),AL1(L'TLJBDTE-1)              
         DC    AL1(TLJBCLI-TLJBD),AL2(TGCLI-TGD),AL1(L'TLJBCLI-1)               
         DC    AL1(TLJBPRD-TLJBD),AL2(TGPRD-TGD),AL1(L'TLJBPRD-1)               
RJBX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RW2X-*,TLW2CDQ,(RW2X-RW2)/FLDLNQ)      W2                    
RW2      DC    AL1(TLW2YEAR-TLW2D),AL2(TGYEAR-TGD),AL1(L'TLW2YEAR-1)            
         DC    AL1(TLW2CUR-TLW2D),AL2(TGTYCUR-TGD),AL1(L'TLW2CUR-1)             
         DC    AL1(TLW2EMP-TLW2D),AL2(TGEMP-TGD),AL1(L'TLW2EMP-1)               
         DC    AL1(TLW2SSN-TLW2D),AL2(TGSSN-TGD),AL1(L'TLW2SSN-1)               
RW2X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RGLX-*,TLGLCDQ,(RGLX-RGL)/FLDLNQ)      GENERAL LIST          
RGL      DC    AL1(TLGLTYPE-TLGLD),AL2(TGLTYP-TGD),AL1(L'TLGLTYPE-1)            
         DC    AL1(TLGLLST-TLGLD),AL2(TGLST-TGD),AL1(L'TLGLLST-1)               
RGLX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RSSX-*,TLSSCDQ,(RSSX-RSS)/FLDLNQ)      SESSION EST.          
RSS      DC    AL1(TLSSTYPE-TLSSD),AL2(TGTYPE-TGD),AL1(L'TLSSTYPE-1)            
         DC    AL1(TLSSAGY-TLSSD),AL2(TGAGY-TGD),AL1(L'TLSSAGY-1)               
         DC    AL1(TLSSEST-TLSSD),AL2(TGEST-TGD),AL1(L'TLSSEST-1)               
RSSX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RECX-*,TLECCDQ,(RECX-REC)/FLDLNQ)      ECAST                 
REC      DC    AL1(TLECCOM-TLECD),AL2(TGCOM-TGD),AL1(L'TLECCOM-1)               
         DC    AL1(TLECEPI-TLECD),AL2(TGEPI-TGD),AL1(L'TLECEPI-1)               
         DC    AL1(TLECSSN-TLECD),AL2(TGSSN-TGD),AL1(L'TLECSSN-1)               
         DC    AL1(TLECSORT-TLECD),AL2(TGCSORT-TGD),AL1(L'TLECSORT-1)           
         DC    AL1(TLECCAT-TLECD),AL2(TGCAT-TGD),AL1(L'TLECCAT-1)               
         DC    AL1(TLECINV-TLECD),AL2(TGINV-TGD),AL1(L'TLECINV-1)               
RECX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(REC2X-*,TLECCCDQ,(REC2X-REC2)/FLDLNQ)  ECAST (SSN)           
REC2     DC    AL1(TLECCSSN-TLECPD),AL2(TGSSN-TGD),AL1(L'TLECCSSN-1)            
         DC    AL1(TLECCEPI-TLECPD),AL2(TGEPI-TGD),AL1(L'TLECCEPI-1)            
         DC    AL1(TLECCCOM-TLECPD),AL2(TGCOM-TGD),AL1(L'TLECCCOM-1)            
         DC    AL1(TLECCCAT-TLECPD),AL2(TGCAT-TGD),AL1(L'TLECCCAT-1)            
         DC    AL1(TLECCINV-TLECPD),AL2(TGINV-TGD),AL1(L'TLECCINV-1)            
REC2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(REPX-*,TLEPCDQ,(REPX-REP)/FLDLNQ)      EPISODE               
REP      DC    AL1(TLEPAGY-TLEPD),AL2(TGAGY-TGD),AL1(L'TLEPAGY-1)               
         DC    AL1(TLEPEPI-TLEPD),AL2(TGEPI-TGD),AL1(L'TLEPEPI-1)               
REPX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RSGX-*,TLSGCDQ,(RSGX-RSG)/FLDLNQ)     SOAP GUARANTEE         
RSG      DC    AL1(TLSGSSN-TLSGD),AL2(TGSSN-TGD),AL1(L'TLSGSSN-1)               
         DC    AL1(TLSGGUA-TLSGD),AL2(TGGUA-TGD),AL1(L'TLSGGUA-1)               
RSGX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RMTX-*,TLMTCDQ,(RMTX-RMT)/FLDLNQ)     MARKET                 
RMT      DC    AL1(TLMTTYPE-TLMTD),AL2(TGMTTYPE-TGD)                            
         DC    AL1(L'TLMTTYPE-1)                                                
         DC    AL1(TLMTCODE-TLMTD),AL2(TGMTCODE-TGD)                            
         DC    AL1(L'TLMTCODE-1)                                                
RMTX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RMT2X-*,TLMTNMDQ,(RMT2X-RMT2)/FLDLNQ) MARKET                 
RMT2     DC    AL1(TLMTNMCD-TLMTPD),AL2(TGMTCODE-TGD)    (NAME)                 
         DC    AL1(L'TLMTNMCD-1)                                                
RMT2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RMT3X-*,TLMTALDQ,(RMT3X-RMT3)/FLDLNQ) MARKET                 
RMT3     DC    AL1(TLMTALAL-TLMTPD),AL2(TGMTALPH-TGD)    (ALPHA)                
         DC    AL1(L'TLMTALAL-1)                                                
RMT3X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RMT4X-*,TLMTICDQ,(RMT4X-RMT4)/FLDLNQ) MARKET                 
RMT4     DC    AL1(TLMTICIC-TLMTPD),AL2(TGMTINTC-TGD)    (INTERNAL)             
         DC    AL1(L'TLMTICIC-1)                                                
RMT4X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCMX-*,TLCMCDQ,(RCMX-RCM)/FLDLNQ)     COMMENT (GUAR)         
RCM      DC    AL1(TLCMAGY-TLCMD),AL2(TGAGY-TGD),AL1(L'TLCMAGY-1)               
         DC    AL1(TLCMSSN-TLCMD),AL2(TGSSN-TGD),AL1(L'TLCMSSN-1)               
         DC    AL1(TLCMGUA-TLCMD),AL2(TGGUA-TGD),AL1(L'TLCMGUA-1)               
         DC    AL1(TLCMTYP-TLCMD),AL2(TGTYPE-TGD),AL1(L'TLCMTYP-1)              
RCMX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCM2X-*,TLCMCDQ,(RCM2X-RCM2)/FLDLNQ)  COMMENT (COMM)         
RCM2     DC    AL1(TLCMAGY-TLCMD),AL2(TGAGY-TGD),AL1(L'TLCMAGY-1)               
         DC    AL1(TLCMCID-TLCMD),AL2(TGCID-TGD),AL1(L'TLCMCID-1)               
         DC    AL1(TLCMICOM-TLCMD),AL2(TGCOM-TGD),AL1(L'TLCMICOM-1)             
         DC    AL1(TLCMVER-TLCMD),AL2(TGVER-TGD),AL1(L'TLCMVER-1)               
         DC    AL1(TLCMTYP-TLCMD),AL2(TGTYPE-TGD),AL1(L'TLCMTYP-1)              
RCM2X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCM3X-*,TLCMCDQ,(RCM3X-RCM3)/FLDLNQ)  COMMENT(CONTR)         
RCM3     DC    AL1(TLCMAGY-TLCMD),AL2(TGAGY-TGD),AL1(L'TLCMAGY-1)               
         DC    AL1(TLCMCNID-TLCMD),AL2(TGCNID-TGD),AL1(L'TLCMCNID-1)            
         DC    AL1(TLCMTRMS-TLCMD),AL2(TGCNTRMS-TGD),AL1(L'TLCMTRMS-1)          
         DC    AL1(TLCMTRME-TLCMD),AL2(TGCNTRME-TGD),AL1(L'TLCMTRME-1)          
         DC    AL1(TLCMTYP-TLCMD),AL2(TGTYPE-TGD),AL1(L'TLCMTYP-1)              
RCM3X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCM5X-*,TLCMCDQ,(RCM5X-RCM5)/FLDLNQ)  COMMENT(INV)           
RCM5     DC    AL1(TLCMAGY-TLCMD),AL2(TGAGY-TGD),AL1(L'TLCMAGY-1)               
         DC    AL1(TLCMINV-TLCMD),AL2(TGINV-TGD),AL1(L'TLCMINV-1)               
         DC    AL1(TLCMTYP-TLCMD),AL2(TGTYPE-TGD),AL1(L'TLCMTYP-1)              
RCM5X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCM6X-*,TLCMCDQ,(RCM6X-RCM6)/FLDLNQ)  COMMENT (ADV)          
RCM6     DC    AL1(TLCMAGY-TLCMD),AL2(TGAGY-TGD),AL1(L'TLCMAGY-1)               
         DC    AL1(TLCMVCID-TLCMD),AL2(TGCID-TGD),AL1(L'TLCMVCID-1)             
         DC    AL1(TLCMVADV-TLCMD),AL2(TGADV-TGD),AL1(L'TLCMVADV-1)             
         DC    AL1(TLCMTYP-TLCMD),AL2(TGTYPE-TGD),AL1(L'TLCMTYP-1)              
RCM6X    EQU   *                                                                
         DC    AL1(RCM4X-*,TLCMCDQ,(RCM4X-RCM4)/FLDLNQ)  COMMENT(FINAL)         
RCM4     DC    AL1(TLCMAGY-TLCMD),AL2(TGAGY-TGD),AL1(L'TLCMAGY-1)               
         DC    AL1(TLCMCID-TLCMD),AL2(TGCID-TGD),AL1(L'TLCMCID-1)               
         DC    AL1(TLCMICOM-TLCMD),AL2(TGCOM-TGD),AL1(L'TLCMICOM-1)             
         DC    AL1(TLCMVER-TLCMD),AL2(TGVER-TGD),AL1(L'TLCMVER-1)               
         DC    AL1(TLCMSEQ-TLCMD),AL2(TGSEQ-TGD),AL1(L'TLCMSEQ-1)               
         DC    AL1(TLCMTYP-TLCMD),AL2(TGTYPE-TGD),AL1(L'TLCMTYP-1)              
RCM4X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RTMX-*,TLTMCDQ,(RTMX-RTM)/FLDLNQ)     TIME                   
RTM      DC    AL1(TLTMSTA-TLTMD),AL2(TGBYTE2-TGD),AL1(L'TLTMSTA-1)             
         DC    AL1(TLTMCOM-TLTMD),AL2(TGCOM-TGD),AL1(L'TLTMCOM-1)               
         DC    AL1(TLTMINV-TLTMD),AL2(TGINV-TGD),AL1(L'TLTMINV-1)               
         DC    AL1(TLTMSSN-TLTMD),AL2(TGSSN-TGD),AL1(L'TLTMSSN-1)               
         DC    AL1(TLTMSORT-TLTMD),AL2(TGCSORT-TGD),AL1(L'TLTMSORT-1)           
         DC    AL1(TLTMCAT-TLTMD),AL2(TGCAT-TGD),AL1(L'TLTMCAT-1)               
RTMX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RVRX-*,TLVRCDQ,(RVRX-RVR)/FLDLNQ)     VERSION                
RVR      DC    AL1(TLVRCOM-TLVRD),AL2(TGCOM-TGD),AL1(L'TLVRCOM-1)               
         DC    AL1(TLVRVER-TLVRD),AL2(TGVER-TGD),AL1(L'TLVRVER-1)               
RVRX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RCNX-*,TLCNCDQ,(RCNX-RCN)/FLDLNQ)     CONTRACT               
RCN      DC    AL1(TLCNAGY-TLCND),AL2(TGAGY-TGD),AL1(L'TLCNAGY-1)               
         DC    AL1(TLCNCNID-TLCND),AL2(TGCNID-TGD),AL1(L'TLCNCNID-1)            
         DC    AL1(TLCNTRMS-TLCND),AL2(TGCNTRMS-TGD),AL1(L'TLCNTRMS-1)          
         DC    AL1(TLCNTRME-TLCND),AL2(TGCNTRME-TGD),AL1(L'TLCNTRME-1)          
RCNX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(ROGX-*,TLOGCDQ,(ROGX-ROG)/FLDLNQ)     COMM'L GROUP           
ROG      DC    AL1(TLOGAGY-TLOGD),AL2(TGAGY-TGD),AL1(L'TLOGAGY-1)               
         DC    AL1(TLOGCLI-TLOGD),AL2(TGCLI-TGD),AL1(L'TLOGCLI-1)               
         DC    AL1(TLOGPRD-TLOGD),AL2(TGPRD-TGD),AL1(L'TLOGPRD-1)               
         DC    AL1(TLOGCOG-TLOGD),AL2(TGCOG-TGD),AL1(L'TLOGCOG-1)               
ROGX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(RPTX-*,TLPTCDQ,(RPTX-RPT)/FLDLNQ)     PRODUCT TYPE           
RPT      DC    AL1(TLPTAGY-TLPTD),AL2(TGAGY-TGD),AL1(L'TLPTAGY-1)               
         DC    AL1(TLPTPRT-TLPTD),AL2(TGPRT-TGD),AL1(L'TLPTPRT-1)               
RPTX     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(MEDX-*,TLMDCDQ,(MEDX-MED)/FLDLNQ)     MEDIA                  
MED      DC    AL1(TLMDTYPE-TLMDD),AL2(TGMEEQU-TGD),AL1(L'TLMDTYPE-1)           
         DC    AL1(TLMDCODE-TLMDD),AL2(TGMDCODE-TGD),AL1(L'TLMDCODE-1)          
MEDX     EQU   *                                                                
*&&DO                                                                           
         DC    AL1(REDX-*,TLEDCDQ,(REDX-RED)/FLDLNQ)     EDIT TYPE              
RED      DC    AL1(TLEDAGY-TLEDD),AL2(TGAGY-TGD),AL1(L'TLEDAGY-1)               
         DC    AL1(TLEDYEAR-TLEDD),AL2(TGEDYR-TGD),AL1(L'TLEDYEAR-1)            
         DC    AL1(TLEDEDT-TLEDD),AL2(TGEDT-TGD),AL1(L'TLEDEDT-1)               
REDX     EQU   *                                                                
*&&                                                                             
         DC    X'FF'               END OF RECTAB                                
         EJECT                                                                  
RECTAB2  DS    0C                                                               
         DC    AL1(PMTX-*,TLPMSCDQ,(PMTX-PMT)/FLDLNQ)     PAYMENT TYPE          
PMT      DC    AL1(TLPMPTYP-TLPMD),AL2(TGPTYP-TGD),AL1(L'TLPMPTYP-1)            
PMTX     EQU   *                                                                
                                                                                
         DC    AL1(TSKX-*,TLTKSCDQ,(TSKX-TSK)/FLDLNQ)     TASK                  
TSK      DC    AL1(TLTKTASK-TLTKD),AL2(TGTASK-TGD),AL1(L'TLTKTASK-1)            
TSKX     EQU   *                                                                
                                                                                
         DC    AL1(TTRX-*,TLTRSCDQ,(TTRX-TTR)/FLDLNQ)     TPROFILE              
TTR      DC    AL1(TLTRAGY-TLTRD),AL2(TGAGY-TGD),AL1(L'TLTRAGY-1)               
         DC    AL1(TLTRCLI-TLTRD),AL2(TGCLI-TGD),AL1(L'TLTRCLI-1)               
         DC    AL1(TLTRTASK-TLTRD),AL2(TGTASK-TGD),AL1(L'TLTRTASK-1)            
         DC    AL1(TLTRPTYP-TLTRD),AL2(TGPTYP-TGD),AL1(L'TLTRPTYP-1)            
TTRX     EQU   *                                                                
                                                                                
         DC    AL1(T4RX-*,TLT4SCDQ,(T4RX-T4R)/FLDLNQ)     T4                    
T4R      DC    AL1(TLT4YEAR-TLT4D),AL2(TGYEAR-TGD),AL1(L'TLT4YEAR-1)            
         DC    AL1(TLT4CUR-TLT4D),AL2(TGCUR-TGD),AL1(L'TLT4CUR-1)               
         DC    AL1(TLT4EMP-TLT4D),AL2(TGEMP-TGD),AL1(L'TLT4EMP-1)               
         DC    AL1(TLT4SSN-TLT4D),AL2(TGSSN-TGD),AL1(L'TLT4SSN-1)               
T4RX     EQU   *                                                                
                                                                                
         DC    AL1(R1RX-*,TLR1SCDQ,(R1RX-R1R)/FLDLNQ)     RL-1                  
R1R      DC    AL1(TLR1YEAR-TLR1D),AL2(TGYEAR-TGD),AL1(L'TLR1YEAR-1)            
         DC    AL1(TLR1CUR-TLR1D),AL2(TGCUR-TGD),AL1(L'TLR1CUR-1)               
         DC    AL1(TLR1EMP-TLR1D),AL2(TGEMP-TGD),AL1(L'TLR1EMP-1)               
         DC    AL1(TLR1SSN-TLR1D),AL2(TGSSN-TGD),AL1(L'TLR1SSN-1)               
R1RX     EQU   *                                                                
                                                                                
         DC    AL1(TARX-*,TLTASCDQ,(TARX-TAR)/FLDLNQ)     T4A-NR                
TAR      DC    AL1(TLTAYEAR-TLTAD),AL2(TGYEAR-TGD),AL1(L'TLTAYEAR-1)            
         DC    AL1(TLTACUR-TLTAD),AL2(TGCUR-TGD),AL1(L'TLTACUR-1)               
         DC    AL1(TLTAEMP-TLTAD),AL2(TGEMP-TGD),AL1(L'TLTAEMP-1)               
         DC    AL1(TLTASSN-TLTAD),AL2(TGSSN-TGD),AL1(L'TLTASSN-1)               
TARX     EQU   *                                                                
                                                                                
         DC    AL1(N4RX-*,TLN4SCDQ,(N4RX-N4R)/FLDLNQ)     NR4                   
N4R      DC    AL1(TLN4YEAR-TLN4D),AL2(TGYEAR-TGD),AL1(L'TLN4YEAR-1)            
         DC    AL1(TLN4CUR-TLN4D),AL2(TGCUR-TGD),AL1(L'TLN4CUR-1)               
         DC    AL1(TLN4EMP-TLN4D),AL2(TGEMP-TGD),AL1(L'TLN4EMP-1)               
         DC    AL1(TLN4SSN-TLN4D),AL2(TGSSN-TGD),AL1(L'TLN4SSN-1)               
N4RX     EQU   *                                                                
                                                                                
         DC    AL1(RPDVX-*,TLPVSCDQ,(RPDVX-RPDV)/FLDLNQ)   ARCH ADVICE          
RPDV     DC    AL1(TLDVAGY-TLDVD),AL2(TGAGY-TGD),AL1(L'TLDVAGY-1)               
         DC    AL1(TLDVCID-TLDVD),AL2(TGCID-TGD),AL1(L'TLDVCID-1)               
         DC    AL1(TLDVADV-TLDVD),AL2(TGADV-TGD),AL1(L'TLDVADV-1)               
RPDVX    EQU   *                                                                
                                                                                
         DC    X'FF'               END OF RECTAB                                
         DROP  RB,R7,R6            DROP BASE & SECONDARY BASE REGS              
         EJECT                                                                  
USEVALR  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)         R3=A(USE CODE), R4=A(USE TYPE)               
         L     R2,TGAUSES          R2=A(USE TABLE)                              
         USING USETABD,R2                                                       
USV2     CLI   0(R2),X'FF'         TEST END OF TABLE                            
         JE    NO                                                               
         TM    0(R1),X'80'         TEST LOOK UP BY EQUATE                       
         JZ    USV4                                                             
         CLC   USEEQU,0(R3)                                                     
         JE    USV8                                                             
         J     USV6                                                             
         SPACE 1                                                                
USV4     CLC   USECDE,0(R3)        ELSE LOOK UP BY CODE                         
         JE    USV8                                                             
         SPACE 1                                                                
USV6     LH    RE,USELEN           BUMP TO NEXT USE ENTRY                       
         AR    R2,RE                                                            
         J     USV2                                                             
         SPACE 1                                                                
USV8     MVC   TGUSCDE,USECDE      MOVE USE CODE TO GLOBAL STORAGE              
         MVC   TGUSEQU,USEEQU      EQUATE                                       
         MVC   TGUSMEDS,USEMEDS    VALID MEDIA FOR USE                          
         MVC   TGUSSTAT,USESTAT    USE STATUS                                   
         MVC   TGUSSTA2,USESTAT2   2ND USE STATUS                               
         MVC   TGUSSTA3,USESTAT3   3RD USE STATUS                               
         MVC   TGUSSTA4,USESTAT4   4TH USE STATUS                               
         MVC   TGUSCYCK,USECYCHK   CYCLE CHECKING BITS                          
         MVC   TGUSXCAT,USEEXCAT   CATEGORY EXCEPTIONS                          
         MVC   TGUSXUNI,USEEXUNI   UNION EXCEPTIONS                             
         MVC   TGUSXUNS,USEEXUN1   EXPANDED UNION EXCEPTIONS                    
         MVC   TGUSNAME,SPACES                                                  
         XR    RE,RE                                                            
         IC    RE,USEDSP           DISPLACEMENT TO SUB-ENTRIES                  
         SHI   RE,USENAME-USETABD+1  LESS DISP. TO NAME + 1                     
         EX    RE,*+8              IS L'NAME - 1                                
         J     *+10                                                             
         MVC   TGUSNAME(0),USENAME NAME                                         
         SPACE 1                                                                
         XC    TGUSTYPD,TGUSTYPD   CLEAR USE TYPE INFO                          
         TM    0(R1),X'40'         TEST DON'T LOOK UP TYPE                      
         JO    USVX                                                             
         IC    RE,USEDSP           NOW BUMP TO FIRST SUB-ENTRY                  
         AR    R2,RE                                                            
         USING USESUBD,R2          R2=A(USE TYPE ENTRIES)                       
         SPACE 1                                                                
USV10    TM    0(R1),X'10'         TEST LOOK UP BY CHARACTER                    
         JZ    USV12                                                            
         CLC   USETYCDE,0(R4)                                                   
         JE    USV16                                                            
         J     USV14                                                            
         SPACE 1                                                                
USV12    CLC   USETYPE,0(R4)       ELSE LOOK UP BY EQUATE                       
         JE    USV16                                                            
         SPACE 1                                                                
USV14    IC    RE,USESBLN          BUMP TO NEXT SUB-ENTRY                       
         AR    R2,RE                                                            
         CLI   0(R2),0             TEST END OF SUB-ENTRIES                      
         JNE   USV10                                                            
         SPACE 1                                                                
         BRAS  RE,GRRTYPE          CALL SPECIAL LOOKUP ROUTINE FOR              
         J     XIT                 RADIO GUARANTEES                             
         SPACE 1                                                                
USV16    MVC   TGUSTYP,USETYPE     SAVE USE TYPE EQUATE IN GLOBAL STOR.         
         MVC   TGUSTYMD,USETYMED   VALID MEDIA FOR TYPE                         
         MVC   TGUSWKS,USEWKS      N'WEEKS IN CYCLE (X'80'==>MTHS)              
         MVC   TGUSTYST,USETYST    TYPE STATUS                                  
         MVC   TGUSTYCD,USETYCDE   TYPE CODE                                    
         SPACE 1                                                                
         IC    RE,USESBLN          L'SUB-ENTRY                                  
         SHI   RE,USETYNME-USESUBD+1 LESS DISP. TO NAME + 1                     
         JM    USVX                = L'NAME - 1                                 
         SPACE 1                                                                
         LA    R1,L'TGUSNAME-1     DETERMINE L'STORAGE AVAILABLE                
         LA    RF,TGUSNAME(R1)     FIND END OF USE NAME                         
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         SPACE 1                                                                
         LA    R0,L'TGUSNAME-3     DETERMINE IF ENOUGH ROOM FOR TYPE            
         SR    R0,R1                                                            
         JM    USVX                                                             
         CR    RE,R0               IF L'NAME IS GT L'STORAGE                    
         JNH   *+6                                                              
         LR    RE,R0               MOVE ONLY FOR L'STORAGE                      
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   2(0,RF),USETYNME    TYPE NAME                                    
         SPACE 1                                                                
USVX     J     YES                                                              
         EJECT                                                                  
*              SYSVAL2 (MORE COMMON ROUTINES)                                   
*                                                                               
         ENTRY SYSVAL2                                                          
         DS    0D                                                               
SYSVAL2  LR    RB,RE                                                            
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING SYSVAL2,RB,R7,R6                                                 
         BR    RF                                                               
         SPACE 3                                                                
YES2     SR    RC,RC                                                            
NO2      LTR   RC,RC                                                            
XIT2     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE A CATEGORY                                   
*                                                                               
*                                  P1 = A(3-BYTE WORK AREA) OR A(EQU)           
*                                  P1 BYTE 0  X'80' = P1 IS EQUATE              
         SPACE 1                                                                
CATVAL   DS    0H                                                               
         L     R2,0(R1)            R2 = A(WORK AREA OR EQUATE)                  
         L     R3,TGACATS          R3 = A(CATEGORY TABLE)                       
         USING CATTABD,R3                                                       
*                                                                               
CAV10    CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         BE    NO2                                                              
*                                                                               
         TM    0(R1),X'80'         IF TEST FOR EQUATE                           
         BZ    CAV20                                                            
         CLC   CATEQU,0(R2)        THEN TEST FOR EQUATE                         
         BE    CAV50                                                            
         B     CAV30                                                            
*                                                                               
CAV20    CLC   CATCDE,0(R2)        ELSE TEST FOR CODE                           
         BE    CAV50                                                            
*                                                                               
CAV30    ZIC   RF,CATLEN           BUMP R3 TO NEXT CATEGORY                     
         AR    R3,RF                                                            
         B     CAV10                                                            
*                                                                               
CAV50    MVC   TGCACDE,CATCDE      SAVE CODE IN GLOBAL                          
         MVC   TGCAT,CATCDE        (THERE ARE TWO PLACES)                       
         MVC   TGCAEQU,CATEQU      SAVE EQUATE IN GLOBAL                        
         MVC   TGCAUNI,CATUNI      SAVE UNIONS IN GLOBAL                        
         MVC   TGCAUNI1,CATUNI1                                                 
         MVC   TGCAUNI2,CATUNI2                                                 
         MVC   TGCAUNI3,CATUNI3                                                 
         MVC   TGCAUNI4,CATUNI4                                                 
         MVC   TGCASTAT,CATSTAT    SAVE STATUS BITS IN GLOBAL                   
         MVC   TGCATYPE,CATTYPE    SAVE CATEGORY TYPE IN GLOBAL                 
         MVC   TGCASORT,CATSORT    SAVE CAST SORT KEY IN GLOBAL                 
*                                                                               
         B     YES2                RETURN EQ                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE A UNION                                      
*                                                                               
*                                  P1 = A(3-BYTE WORK AREA) OR A(EQU)           
*                                  P1 BYTE 0  X'80' = P1 IS 1-BYTE EQU          
*                                             X'40' = P1 IS 4-BYTE EQU          
*                                             X'20' = P1 IS ESTIMA EQU          
         SPACE 1                                                                
UNIVAL   DS    0H                                                               
         L     R2,0(R1)            R2 = A(WORK AREA OR EQUATE)                  
         L     R3,TGAUNIS          R3 = A(UNION TABLE)                          
         USING UNITABD,R3                                                       
*                                                                               
UNV10    CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         BE    NO2                                                              
*                                                                               
         TM    0(R1),X'80'         IF TEST FOR 1-BYTE EQUATE                    
         BZ    UNV20                                                            
         CLC   UNIEQU,0(R2)        THEN TEST FOR 1-BYTE EQUATE                  
         BE    UNV50                                                            
         B     UNV30                                                            
*                                                                               
UNV20    TM    0(R1),X'40'         IF TEST FOR 4-BYTE EQUATE                    
         BZ    UNV22                                                            
         CLC   UNIEQUS,0(R2)       THEN TEST FOR 4-BYTE EQUATE                  
         BE    UNV50                                                            
         B     UNV30                                                            
*                                                                               
UNV22    TM    0(R1),X'20'         IF TEST FOR ESTIMATE EQUATE                  
         BZ    UNV25                                                            
         CLC   UNIESUNI,0(R2)      THEN TEST FOR ESTIMATE EQUATE                
         BE    UNV50                                                            
         B     UNV30                                                            
*                                                                               
UNV25    CLC   UNICDE,0(R2)        ELSE TEST FOR CODE                           
         BE    UNV50                                                            
*                                                                               
UNV30    LA    R3,UNILNQ(R3)       BUMP R3 TO NEXT UNION                        
         B     UNV10                                                            
*                                                                               
UNV50    MVC   TGUNCDE,UNICDE      SAVE CODE IN GLOBAL                          
         MVC   TGUNI,UNICDE        (THERE ARE 2 PLACES)                         
         MVC   TGUNEQU,UNIEQU      SAVE EQUATE IN GLOBAL                        
         MVC   TGUNEQUS,UNIEQUS                                                 
         MVC   TGUNYR,UNIYR        SAVE YEARS IN GLOBAL                         
         MVC   TGESUNI,UNIESUNI    SAVE ESTIMATE UNION EQUATE                   
*                                                                               
         B     YES2                RETURN EQ                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE A CONTRACT YEAR                              
*                                                                               
*                                  P1 = A(3-BYTE WORK AREA) OR A(EQU)           
*                                  P1 BYTE 0  X'80' = P1 IS EQUATE              
*                                  P1 BYTE 0  X'40' = CHECK UNION               
         SPACE 1                                                                
YRVAL    DS    0H                                                               
         L     R2,0(R1)            R2 = A(WORK AREA OR EQUATE)                  
         MVC   TGBYTE4,0(R1)        ''  PARAMETER BITS                          
         L     R3,TGAYEARS         R3 = A(YEARS TABLE)                          
         USING YRTABD,R3                                                        
*                                                                               
YRV10    CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         BE    NO2                                                              
*                                                                               
         TM    TGBYTE4,X'80'       IF TEST FOR EQUATE                           
         BZ    YRV20                                                            
*                                                                               
         CLC   YREQU,0(R2)         THEN TEST FOR EQUATE                         
         BNE   YRV30                                                            
*                                                                               
         TM    TGBYTE4,X'40'       IF CHECKING UNION                            
         BZ    YRV50                                                            
         GOTOR UTEST,DMCB,(X'80',YRUNI1),TGUNEQUS  MATCH ON UNION               
         BZ    YRV30                                                            
         B     YRV50                                                            
*                                                                               
YRV20    CLC   YRCDE,0(R2)         ELSE TEST FOR CODE                           
         BE    YRV50                                                            
*                                                                               
YRV30    LA    R3,YRLNQ(R3)        BUMP R3 TO NEXT CATEGORY                     
         B     YRV10                                                            
*                                                                               
YRV50    MVC   TGYRCDE,YRCDE       SAVE CODE IN GLOBAL                          
         MVC   TGYREQU,YREQU       SAVE EQUATE IN GLOBAL                        
         ST    R3,TGAYEAR          SAVE ADDRESS YEARTAB ENTRY                   
*                                                                               
         B     YES2                RETURN EQ                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE OR FIND AN UPGRADE CODE                      
*                                                                               
*                                  P1 = A(3-BYTE UPGRADE CODE)                  
*                                  P2 = A(1-BYTE UPGRADE TYPE)                  
*                                  P1 BYTE 0 X'80' = FIND UPGRADE CODE          
*                                  P1 = A(3-BYTE 'FROM' CODE)                   
*                                  P2 = A(1-BYTE 'FROM' TYPE)                   
*                                  P3 = A(3-BYTE 'TO' CODE)                     
*                                  P4 = A(1-BYTE 'TO' TYPE)                     
         SPACE 1                                                                
         USING UPGRDD,R4                                                        
UPGRVAL  DS    0H                                                               
         L     R4,TGAUPGRS         R4 = A(UPGRADE TABLE)                        
         LM    R2,R3,0(R1)         R2 = A(CODE), R3=A(TYPE)                     
         TM    0(R1),X'80'         TEST VALIDATING UPGRADE CODE                 
         BO    UPV20                                                            
*                                                                               
UPV10    CLI   0(R4),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         BE    NO2                                                              
         CLC   UPGCDE,0(R2)        ELSE TEST FOR CODE                           
         BNE   *+14                                                             
         CLC   UPGTYP,0(R3)        AND TYPE                                     
         BE    UPV50                                                            
         LA    R4,UPGLNQ(R4)       BUMP TO NEXT                                 
         B     UPV10                                                            
*                                  FINDING UPGRADE CODE                         
UPV20    LM    RE,RF,8(R1)         RE = A('TO CODE), RF=A('TO' TYPE)            
*                                                                               
UPV22    CLI   0(R4),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         BE    NO2                                                              
         CLC   UPGFRCD,0(R2)       ELSE TEST FOR 'FROM' CODE                    
         BNE   UPV25                                                            
         CLC   UPGFRTY,0(R3)       AND 'FROM' TYPE                              
         BNE   UPV25                                                            
         CLC   UPGTOCD,0(RE)       ELSE TEST FOR 'TO' CODE                      
         BNE   UPV25                                                            
         CLC   UPGTOTY,0(RF)       AND 'TO' TYPE                                
         BE    UPV50                                                            
UPV25    LA    R4,UPGLNQ(R4)       BUMP TO NEXT                                 
         B     UPV22                                                            
*                                                                               
UPV50    MVC   TGUPCDE,UPGCDE      SAVE UPGRADE USE CODE IN GLOBAL              
         MVC   TGUPTYP,UPGTYP      UPGRADE USE TYPE                             
         MVC   TGUPFRCD,UPGFRCD    'FROM' USE CODE                              
         MVC   TGUPFRTY,UPGFRTY    'FROM' USE TYPE                              
         MVC   TGUPTOCD,UPGTOCD    'TO' USE CODE                                
         MVC   TGUPTOTY,UPGTOTY    'TO' USE TYPE                                
*                                                                               
         B     YES2                RETURN EQ                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE A MAJOR CODE                                 
*                                                                               
*                                  P1 = A(9-BYTE WORK AREA) OR A(EQU)           
*                                  P1 BYTE 0  X'80' = P1 IS EQUATE              
         SPACE 1                                                                
         USING MAJTABD,R3                                                       
MAJVALL  NTR1                                                                   
MAJVAL   DS    0H                                                               
         L     R2,0(R1)            R2 = A(WORK AREA OR EQUATE)                  
         L     R3,TGAMAJS          R3 = A(MAJORS TABLE)                         
*                                                                               
         CLI   TGUSEQU,UFGR        IF FGR USE (ASSUME NEW MAJORS TYPE)          
         BNE   MAV10                                                            
         TM    0(R1),X'80'         AND TEST FOR EQUATE                          
         BZ    LOCVAL                                                           
         L     R3,TGALOCS          R3 = A(LOCATIONS TABLE)                      
*                                                                               
MAV10    CLI   0(R3),X'FF'         IF END OF TABLE                              
         BE    NO2                 THEN RETURN NEQ                              
*                                                                               
         TM    0(R1),X'80'         IF TEST FOR EQUATE                           
         BZ    MAV20                                                            
         CLC   MAJEQU,0(R2)        THEN TEST FOR EQUATE                         
         BE    MAV50                                                            
         B     MAV30                                                            
*                                                                               
MAV20    CLC   MAJCHAR,0(R2)       ELSE TEST FOR CHARACTER MAJOR CODE           
         BE    MAV50                                                            
*                                                                               
MAV30    LA    R3,MAJNEXT          BUMP R3 TO NEXT MAJOR CODE                   
         B     MAV10                                                            
*                                                                               
MAV50    MVC   TGMAEQU,MAJEQU      SAVE EQUATE IN GLOBAL                        
         MVC   TGMACHAR,MAJCHAR    SAVE CHARACTER MAJOR CODE IN GLOBAL          
*                                                                               
         B     YES2                RETURN EQ                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE LOCATION BASED ON CHARACTER                  
*                                  R2 = A(9-BYTE LOCATION)                      
         SPACE 1                                                                
         USING MAJTABD,R3                                                       
LOCVAL   DS    0H                                                               
         L     R2,0(R1)            R2 = A(LOCATION)                             
         CLI   8(R2),C'+'          MAKE SURE DOESN'T END IN A +                 
         BE    NO2                                                              
*                                                                               
         MVI   TGBYTE2,0           PRE-CLEAR EQUATE                             
         BAS   RE,CHKMULT          RETURNS R4 = (N'LOCATIONS)                   
         STC   R4,TGBYTE                                                        
*                                                                               
LOCV5    L     R3,TGALOCS          R3=A(LOCATIONS TABLE)                        
         BAS   RE,SETLOCLN         RETURNS R1=(L'LOCATION)                      
         LTR   R1,R1               PROTECT AGAINST BAD LOCATIONS                
         BZ    NO2                                                              
         BCTR  R1,0                SET LENGTH FOR EXECUTED COMPARE              
*                                                                               
LOCV10   CLI   0(R3),X'FF'         TEST END OF TABLE                            
         BE    NO2                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   MAJCHAR(0),0(R2)    COMPARE BASED ON INPUT LENGTH                
         BE    *+12                                                             
         LA    R3,MAJNEXT          BUMP R3 TO NEXT MAJOR CODE                   
         B     LOCV10                                                           
*                                                                               
         OC    TGBYTE2,MAJEQU      EVERYTHING OK - SET BIT ON                   
         CLI   TGBYTE,0            IF MULTIPLE LOCATIONS                        
         BE    LOCVYES                                                          
         AR    R2,R1               BUMP TO NEXT LOCATION                        
         LA    R2,2(R2)                                                         
         BCT   R4,LOCV5            AND LOOP                                     
*                                                                               
LOCVYES  GOTO1 MAJVALL,DMCB,(X'80',TGBYTE2)  SET EQUATE AND CHARACTER           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     YES2                                                             
         EJECT                                                                  
*              ROUTINE RETURNS R4 EQUAL TO NUMBER OF LOCATIONS                  
*              FOUND IN CHARACTER EXPRESSION                                    
*                                  R2 = A(LOCATION EXPRESSION)                  
CHKMULT  NTR1                                                                   
         LA    R4,1                PRE-SET R4=(N'LOCATIONS)                     
         LA    R0,L'TGMACHAR                                                    
*                                                                               
CHKMULT5 CLI   0(R2),C'+'          IF SEPERATOR FOUND                           
         BNE   *+8                                                              
         LA    R4,1(R4)            ADD TO LOCATIONS                             
         LA    R2,1(R2)                                                         
         BCT   R0,CHKMULT5                                                      
*                                                                               
         XIT1  REGS=(R4)           RETURN R4                                    
         SPACE 2                                                                
*              ROUTINE RETURNS R1 EQUAL TO LENGTH OF LOCATION                   
*                                  R2=A(LOCATION)                               
SETLOCLN NTR1                                                                   
         XR    R1,R1               PRE-CLEAR R1=A(L'LOCATION)                   
         LA    R0,L'TGMACHAR                                                    
*                                                                               
SETLOC5  CLI   0(R2),C' '          LOOP TILL SPACE OR PLUS SIGN                 
         BE    SETLOCX                                                          
         CLI   0(R2),C'+'                                                       
         BE    SETLOCX                                                          
         LA    R1,1(R1)            ADD TO LENGTH OF LOCATION                    
         LA    R2,1(R2)            AND BUMP TO NEXT CHAR IN EXPRESSION          
         BCT   R0,SETLOC5                                                       
*                                                                               
SETLOCX  XIT1  REGS=(R1)           RETURN R1                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE A TAX UNIT CODE                              
*                                                                               
*                                  P1 BYTE 0 = L'TAX UNIT CODE OR               
*                                              X'FF' = CHECK AGAINST            
*                                                      TGCTRY                   
*                                     BYTES 1-3 = A(WORK AREA)                  
*                                                                               
         SPACE 1                                                                
TAXVAL   DS    0H                                                               
         BRAS  RE,TXVALRT                                                       
         BE    YES2                                                             
         B     NO2                                                              
*              ROUTINE TO VALIDATE A BILLING TYPE                               
*                                                                               
*                                  P1 = A(BILLING TYPE)                         
         SPACE 1                                                                
BTYPVAL  DS    0H                                                               
         L     R2,0(R1)                                                         
         L     R3,TGABTYP                                                       
         USING BTYPTABD,R3                                                      
*                                                                               
BTY10    CLI   0(R3),X'FF'                                                      
         BE    NO2                                                              
         CLC   BTYPCDE,0(R2)       VALID BILLING TYPE                           
         BE    BTY20                                                            
         LA    R3,BTYPNEXT                                                      
         B     BTY10                                                            
*                                                                               
BTY20    MVC   TGBTYPE,BTYPCDE     BILLING TYPE CODE                            
         MVC   TGBTYPO,BTYPPOTY    POOL TYPE                                    
         MVC   TGBTSTAT,BTYPSTAT   STATUS                                       
         MVC   TGBSRC,BTYPSRC      STANDARD RATES                               
         B     YES2                                                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE AN APPLIED CODE                              
*                                                                               
*                                 P1  BYTES 0-3 = A(1 BYTE APPL CODE)           
         SPACE 1                                                                
APPLVAL  DS    0H                                                               
         L     R2,0(R1)            R2 = A(LOOKUP APPLIED CODE)                  
         L     R3,TGAAPPL                                                       
         USING APPLTABD,R3                                                      
*                                                                               
APP10    CLI   0(R3),X'FF'                                                      
         BE    NO2                                                              
         CLC   APPLCDE,0(R2)       VALID APPLIED CODE                           
         BE    APP20                                                            
         LA    R3,APPLNEXT                                                      
         B     APP10                                                            
*                                                                               
APP20    MVC   TGAPCDE,APPLCDE     APPLIED CODE                                 
         MVC   TGAPTEXT,APPLTEXT   APPLIED TEXT                                 
         B     YES2                                                             
         EJECT                                                                  
*              ROUTINE TO OUTPUT INVOICE ERROR MESSAGE                          
         SPACE 1                                                                
*                                  P1, BYTE  0   = ERROR CODE                   
*                                      BYTES 1-3 = A(OUTPUT FIELD)              
*                                                                               
ERROUT   DS    0H                                                               
         L     R2,0(R1)            A(OUTPUT FIELD)                              
         MVC   BYTE,0(R1)          ERROR CODE                                   
         L     R3,TGAIERRS         A(ERROR TABLE)                               
         USING ERRTYPSD,R3                                                      
         MVC   0(L'ERDESC,R2),SPACES CLEAR FIELD                                
         SPACE 1                                                                
ERR10    CLI   ERCODE,X'FF'        END OF TABLE                                 
         BE    XIT2                                                             
         CLC   ERCODE,BYTE         IF THE ERROR IS NOT FOUND                    
         BE    ERR20                                                            
         LA    R3,ERRNXT(R3)       BUMP TABLE                                   
         B     ERR10                                                            
         SPACE 1                                                                
ERR20    MVC   0(L'ERDESC,R2),ERDESC  ELSE MOVE DESCRIPTION TO OUTPUT           
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE A MUSIC LICENSER CODE                        
*                                                                               
*                                  P1 = A(9-BYTE WORK AREA) OR A(EQU)           
*                                  P1 BYTE 0  X'80' = P1 IS EQUATE              
         SPACE 1                                                                
LICVAL   DS    0H                                                               
         L     R2,0(R1)            R2 = A(WORK AREA OR EQUATE)                  
         L     R3,TGALICS          R3 = A(LICENSER TABLE)                       
         USING LICTABD,R3                                                       
*                                                                               
LICV10   CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         BE    NO2                                                              
*                                                                               
         TM    0(R1),X'80'         IF TEST FOR EQUATE                           
         BZ    LICV20                                                           
         CLC   LICCDE,0(R2)        THEN TEST FOR EQUATE                         
         BE    LICV40                                                           
         B     LICV30                                                           
*                                                                               
LICV20   CLC   LICNAME,0(R2)       ELSE TEST FOR NAME                           
         BE    LICV40                                                           
*                                                                               
LICV30   LA    R3,LICNEXT          BUMP R3 TO NEXT LICENSER CODE                
         B     LICV10                                                           
*                                                                               
LICV40   MVC   TGLCCDE,LICCDE      SAVE LICENSER CODE IN GLOBAL                 
         MVC   TGLCNAME,LICNAME    SAVE LICENSER NAME IN GLOBAL                 
*                                                                               
         B     YES2                RETURN EQ                                    
         EJECT                                                                  
*              ROUTINE TO OUTPUT ADJUSTMENT NAMES                               
         SPACE 1                                                                
*                                  P1, BYTE  0   = ADJUSTMENT CODE              
*                                      BYTES 1-3 = A(OUTPUT FIELD)              
*                                                                               
ADJOUT   DS    0H                                                               
         L     R2,0(R1)            A(OUTPUT FIELD)                              
         MVC   BYTE,0(R1)          ADJUSTMENT CODE                              
         L     R3,TGADJST          A(ADJUSTMENT TABLE)                          
         USING ADJTYPSD,R3                                                      
         MVC   0(L'ADJDESC,R2),SPACES     CLEAR FIELD                           
         SPACE 1                                                                
ADJ10    CLI   ADJCODE,X'FF'       END OF TABLE                                 
         BE    ADJ20                                                            
         CLC   ADJCODE,BYTE        IF THE ADJUSTMENT IS NOT FOUND               
         BE    ADJ20                                                            
         LA    R3,ADJNXT(R3)       BUMP TABLE                                   
         B     ADJ10                                                            
         SPACE 1                                                                
ADJ20    MVC   0(L'ADJDESC,R2),ADJDESC  ELSE MOVE DESCRIPTION TO OUTPUT         
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO DISPLAY CAST ERROR DESCRIPTION                        
         SPACE 1                                                                
*                                  P1, BYTE  0   = ERROR CODE                   
*                                      BYTES 1-3 = A(OUTPUT FIELD)              
*                                                                               
         USING CERRTYPD,R3                                                      
CERROUT  DS    0H                                                               
         L     R2,0(R1)            A(OUTPUT FIELD)                              
         MVC   BYTE,0(R1)          ERROR CODE                                   
         L     R3,TGACERRS         A(ERROR TABLE)                               
         MVC   0(L'CERDESC,R2),SPACES CLEAR FIELD                               
         SPACE 1                                                                
CERR10   CLI   CERCODE,X'FF'       END OF TABLE                                 
         BE    XIT2                                                             
         CLC   CERCODE,BYTE        IF THE ERROR IS NOT FOUND                    
         BE    CERR20                                                           
         LA    R3,CERRNXT(R3)      BUMP TABLE                                   
         B     CERR10                                                           
         SPACE 1                                                                
CERR20   MVC   0(L'CERDESC,R2),CERDESC ELSE MOVE DESCRIPTION TO OUTPUT          
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE UPDATES PASSWORD ELEMENTS (TAPWD) ON STAFF               
*              RECORD IN AIO IF NEW PASSWORD IS VALID (NOT ANY                  
*              OF THE LAST SIX PASSWORDS)                                       
*                                                                               
*                                  P1 = A(NEW PASSWORD)                         
         SPACE 1                                                                
PWRDVAL  DS    0H                                                               
         L     R2,0(R1)            R2 = A(NEW PASSWORD)                         
         L     R3,AIO              R3 = A(STAFF RECORD)                         
*                                                                               
         USING TAPWD,R3                                                         
         MVI   ELCODE,TAPWELQ      GET PASSWORD ELEMENT                         
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
PWRDVAL2 BAS   RE,NEXTEL2                                                       
         BNE   PWRDVAL4                                                         
         CLC   TAPWPWD,0(R2)       CHECK MATCH ON A PREVIOUS PASSWORD           
         BE    NO2                 YES - ERROR                                  
         B     PWRDVAL2            NO - CHECK NEXT ELEMENT                      
*                                                                               
PWRDVAL4 XC    ELEM,ELEM           NEW PASSWORD OKAY                            
         LA    R3,ELEM             ADD NEW TAPWD ELEMENT                        
         MVI   TAPWEL,TAPWELQ      SET ELEMENT TYPE                             
         MVI   TAPWLEN,TAPWLNQ     SET ELEMENT LENGTH                           
         MVC   TAPWDTE,TGTODAY1    SET DATE OF PASSWORD CHANGE                  
         XC    TAPWDTE,HEXFFS2     COMPLEMENTED                                 
         TIME  DEC                                                              
         STCM  R0,14,TAPWTIM       SET TIME OF PASSWORD CHANGE                  
         XC    TAPWTIM,HEXFFS2     COMPLEMENTED                                 
         MVC   TAPWPWD,0(R2)       SET NEW PASSWORD                             
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         XR    R1,R1               R1=COUNT OF ELEMENTS                         
         L     R3,AIO              R3=A(STAFF RECORD)                           
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
PWRDVAL6 BAS   RE,NEXTEL2                                                       
         BNE   PWRDVAL8                                                         
*                                                                               
         LA    R1,1(R1)            INCREMENT ELEMENT COUNT                      
         CHI   R1,6                ONLY NEED TO REMEMBER SIX PASSWRDS           
         BNH   PWRDVAL6                                                         
         MVI   TAPWEL,X'FF'        MARK ELEMENT FOR DELETION                    
         B     PWRDVAL6                                                         
*                                                                               
PWRDVAL8 MVI   ELCODE,X'FF'        DELETE MARKED ELEMENTS                       
         GOTO1 REMELEM                                                          
         B     YES2                                                             
         EJECT                                                                  
*              ROUTINE CONVERTS DDS TIME TO STANDARD EASTERN TIME               
*                                                                               
*                                  P1 = A(INPUT TIME - DDS TIME)                
*                                  P2 = A(INPUT DATE - PWOS)                    
*                                  P3, BYTE 0 = LENGTH OF OUTPUT                
*                                  P3 = A(OUTPUT AREA)                          
         SPACE 1                                                                
TIMECON  DS    0H                                                               
         LM    R2,R4,0(R1)         R2=A(TIME), R3=A(DATE), R4=A(OUTPUT)         
         ZIC   R5,8(R1)            R5 = L'OUTPUT AREA                           
         SPACE 1                                                                
         MVO   TGDUB,0(3,R2)       MAKE TIME PACKED                             
         OI    TGDUB+7,X'0F'                                                    
         CLC   =X'951218',0(R3)    IF DATE IS BEFORE 12/18/95                   
         BNH   *+14                                                             
         AP    TGDUB,=P'80000'     ADD 8 HOURS TO DDS TIME                      
         B     TIMECON2                                                         
         THMS  DDSTIME=YES         ELSE, ADD TIME DIFF. RETURNED IN R0          
         ST    R0,TGFULL                                                        
         AP    TGDUB,TGFULL(4)                                                  
         SPACE 1                                                                
TIMECON2 CP    TGDUB+4(4),=P'240000'                                            
         BL    *+10                ADJUST TO 24 HOURS IF NECESSARY              
         SP    TGDUB+4(4),=P'240000'                                            
         OI    TGDUB+7,X'0F'                                                    
         UNPK  WORK(7),TGDUB+4(4)                                               
         SPACE 1                                                                
         CHI   R5,4                IF LENGTH EQUAL TO 4                         
         BNE   TIMECON5                                                         
         MVC   0(2,R4),WORK+1      DISPLAY HOURS                                
         MVC   2(2,R4),WORK+3      DISPLAY HHMM                                 
         B     TIMECONX                                                         
         SPACE 1                                                                
TIMECON5 MVC   0(2,R4),WORK+1      DISPLAY HOURS                                
         CHI   R5,2                IF L'OUTPUT NOT TWO                          
         BE    TIMECONX                                                         
         MVI   2(R4),C':'                                                       
         MVC   3(2,R4),WORK+3      DISPLAY MINUTES TOO                          
         CHI   R5,5                IF L'OUTPUT NOT FIVE                         
         BE    TIMECONX                                                         
         MVI   5(R4),C':'                                                       
         MVC   6(2,R4),WORK+5      DISPLAY SECONDS TOO                          
TIMECONX B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES THAT THE USE TYPE IS VALID FOR THE           *         
*        COMMERCIAL TYPE                                              *         
*        ON ENTRY ... P1 = A(COMMERCIAL TYPE)                         *         
*                     R4 = A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
VALTYPT  NTR1                                                                   
VALTYP   BRAS  RE,VTYPE                                                         
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT THE USE TYPE IS VALID FOR THE           *         
*        COMMERCIAL TYPE                                              *         
*        ON ENTRY ... P1 = A(COMMERCIAL TYPE)                         *         
*                     R4 = A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
UNITESTT NTR1                                                                   
UNITEST  BRAS  RE,UTEST                                                         
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE TO PACK SSN INTO 6 BYTE BASE 36 NUMBER               *         
***********************************************************************         
                                                                                
SSNPACKT NTR1                                                                   
SSNPACK  BRAS  RE,SSN2PID                                                       
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE TO UNPACK 6 CHAR BASE 36 NUMBER INTO 9-CHARACTER     *         
*        SOCIAL SECURITY NUMBER                                       *         
*        ON ENTRY ...  P1 = A(6 BYTE BASE 36 NUMBER)                  *         
***********************************************************************         
                                                                                
SSNUNPKT NTR1                                                                   
SSNUNPK  BRAS  RE,SSNUPK                                                        
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE TO ADD ERROR DETAILS TO ERROR TABLE                  *         
*        ON ENTRY ... P1 = A(ERROR ENTRY TO ADD)                      *         
***********************************************************************         
                                                                                
ADDERROT NTR1                                                                   
ADDERROR BRAS  RE,AERROR                                                        
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE COUNTRY CODE                             *         
*        ON ENTRY ... P1 = A(COUNTRY SCREEN FIELD HEADER)             *         
***********************************************************************         
                                                                                
VALCTRYT NTR1                                                                   
VALCTRY  BRAS  RE,VCTRY                                                         
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE TO TRANSLATE AGENT CODE                              *         
*        ON ENTRY ... P1 BYTE 0 = X'80' CHARACTER TO NUMERIC          *         
*                                 X'40' NUMERIC TO CHARACTER          *         
*                     P1        = A(INCOMING AGENT CODE)              *         
*                     P2        = A(TRANSLATED AGENT CODE)            *         
***********************************************************************         
                                                                                
TRNSAGTT NTR1                                                                   
TRNSAGT  BRAS  RE,TAGT                                                          
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE COPIES NEW-STYLE OVERSCALE 2ND PERCENT               *         
*        INTO CAST DETAILS ELEMENT                                    *         
*        ON ENTRY ... P1 = A(CAST DETAILS ELEMENT)                    *         
*                     P2 = A(CAST RECORD)                             *         
*                     P3 = A(USE TYPE)                                *         
***********************************************************************         
                                                                                
SETOV2T  NTR1                                                                   
SETOV2   BRAS  RE,SOV2                                                          
         B     XIT2                                                             
                                                                                
***********************************************************************         
*        ROUTINE TO CHECK IF CHECK LOCKOUT ACTIVE                     *         
*        INTO CAST DETAILS ELEMENT                                    *         
*        RETURN ... CC = EQUAL, CHECK LOCKOUT ACTIVE                  *         
*                      = NOT EQUAL, NOT ACTIVE                        *         
***********************************************************************         
                                                                                
TSTLCKTT NTR1                                                                   
TSTLCKT  BRAS  RE,TSTLCKTA                                                      
         B     XIT2                                                             
                                                                                
SETLCKTT NTR1                                                                   
SETLCKT  BRAS  RE,SETLCKTA                                                      
         B     XIT2                                                             
                                                                                
         GETEL2 (R3),DATADISP,ELCODE                                            
         SPACE 3                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
HEXFFS2  DC    6X'FF'                                                           
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R6,R7                                                            
         EJECT                                                                  
TXVALRT  NTR1  BASE=*,LABEL=*      R2 = A(WORK AREA)                            
         L     R2,0(R1)            R2 = A(WORK AREA)                            
                                                                                
         CLI   OFFLINE,C'N'        IF RUNNING ONLINE                            
         JNE   TAV00                                                            
         CLC   =C'LAX',0(R2)       LAX IS NO LONGER ACCEPTED                    
         JE    TAVINV                                                           
                                                                                
TAV00    CLI   0(R1),X'FF'         IF CHECKING AGAINST TGCNTRY                  
         JNE   TAV10                                                            
         OC    TGCTRY,TGCTRY       ENSURE TGCNTRY IS SET                        
         JZ    TAVINV                                                           
         CLC   TGCTRY,=C'US'       IF TGCTRY IS NOT US                          
         JNE   TAV40               SKIP AHEAD                                   
         MVI   0(R1),3                                                          
                                                                                
         USING TALUNITD,R3                                                      
TAV10    L     R3,TGAUNITS         R3 = A(TAX UNIT TABLE)                       
         ZIC   RF,0(R1)            RF = L'TAX UNIT                              
         BCTR  RF,0                                                             
                                                                                
TAV20    CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         JE    TAVINV                                                           
                                                                                
         CLI   TALUCODE+2,C' '     IF NOT CITY THEN SAVE POTENTIAL              
         JH    *+10                    STATE FOR CITY                           
         MVC   TGTASTCY,TALUCODE                                                
                                                                                
         EX    RF,*+8              IF CODE MATCHES TABLE THEN DONE              
         J     *+10                                                             
         CLC   TALUCODE(0),0(R2)   THEN                                         
         JE    TAV30                                                            
                                                                                
         LA    R3,TALUNEXT         ELSE BUMP R3 TO NEXT TAX UNIT                
         J     TAV20                                                            
                                                                                
TAV30    MVC   TGTACODE,TALUCODE   SAVE CODE IN GLOBAL                          
         MVC   TGTANAME,TALUNAME        NAME                                    
         MVC   TGTAGATE,TALUGATE        GATES/MCDONALD STATE NO.                
         MVC   TGTANUM,TALUNUM          NUMERIC STATE CODE                      
         MVC   TGTASTAT,TALUSTAT        STATUS BYTE                             
         MVC   TGTAZIPF,TALUZIPF        FIRST 3 ZIP DIGITS FROM                 
         MVC   TGTAZIPT,TALUZIPT        FIRST 3 ZIP DIGITS TO                   
         J     YES2                RETURN EQ                                    
         DROP  R3                                                               
                                                                                
         USING CTRYTABD,R3                                                      
TAV40    L     R3,TGACTRYS                                                      
TAV50    CLC   CTRYCODE,TGCTRY     FIND COUNTRY CODE IN COUNTRY TABLE           
         JE    TAV60                                                            
         ZIC   R0,CTRYLEN                                                       
         AR    R3,R0                                                            
         CLI   0(R3),X'FF'                                                      
         JNE   TAV50                                                            
         DC    H'00'                                                            
                                                                                
TAV60    CLC   0(L'CTRYPROV,R2),SPACES   IF PROVINCE NOT PROVIDED               
         JNE   TAV70                                                            
         CLI   CTRYDSP,0                 BUT IS REQUIRED FOR COUNTRY            
         JE    YES2                                                             
         J     TAVMIS                    RETURN MISSING ERROR                   
                                                                                
TAV70    CLI   CTRYDSP,0           IF PROVINCE IS PROVIDED                      
         JNE   TAV80               BUT IS NOT ALLOWED FOR COUNTRY               
         J     TAVINV              RETURN NEGATIVE CONDITION CODE               
                                                                                
         USING CTRYSUBD,R1                                                      
TAV80    ZIC   R1,CTRYDSP                                                       
         AR    R1,R3                                                            
         ZIC   R0,CTRYLEN                                                       
         AR    R0,R3                                                            
TAV90    CLC   CTRYPROV,0(R2)      IF PROVINCE IS VALID FOR COUNTRY             
         JNE   TAV100                                                           
         MVC   TGTANAME,CTRYPNAM   PROVINCE NAME                                
         MVI   TGTASTAT,0                                                       
         ST    R1,TGACTRY          RETURN A(PROVINCE ENTRY)                     
         J     YES2                AND POSITIVE CONDITION CODE                  
TAV100   LA    R1,CRTYSLNQ(R1)                                                  
         CR    R1,R0                                                            
         JL    TAV90                                                            
         J     TAVINV              ELSE RETURN NEGATIVE CONDITION CODE          
         DROP  R1,R3                                                            
                                                                                
TAVINV   MVI   ERROR,INVALID                                                    
         J     NO2                                                              
                                                                                
TAVMIS   MVI   ERROR,INVALID                                                    
         J     NO2                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY CHECK LOCKOUT STATUS                          
         SPACE 1                                                                
DISPLOCK NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CONHED2          FIND FIRST AVAILABLE SLOT                    
         LA    R0,L'CONHED2-23                                                  
         XR    R4,R4                                                            
         SPACE 1                                                                
DLCK2    CLC   0(22,R2),=C'(CHECK LOCKOUT ACTIVE)'  OR PREV. LITERAL            
         BNE   *+10                                                             
         MVC   0(22,R2),SPACES                                                  
*                                                                               
         CLI   0(R2),C' '          SCAN FOR FIRST OPEN SPOT                     
         BH    *+12                                                             
         LTR   R4,R4               IF NOT SAVED YET                             
         BNZ   *+6                                                              
         LR    R4,R2               SAVE IT NOW                                  
*                                                                               
         LA    R2,1(R2)            TRY NEXT                                     
         BCT   R0,DLCK2                                                         
*                                                                               
         GOTOR TSTLCKTA,DMCB,=CL10'TAL_CHECKS'                                  
         BE    DLCK50                                                           
         GOTOR TSTLCKTA,DMCB,=CL10'TAL_PRCHKS'                                  
         BE    DLCK50                                                           
         GOTOR TSTLCKTA,DMCB,=CL10'TAL_P+CHKS'                                  
         BNE   DLCK99                                                           
DLCK50   MVC   0(22,R4),=C'(CHECK LOCKOUT ACTIVE)'  DISPLAY MESSAGE             
DLCK99   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DETERMINE EXPIRATION DATE (DEFAULT IS 21 MONTHS)                 
*                        P1 = A(TACOEL)                                         
*                        P2 = A(OVERRIDE BASE DATE)                             
*                        P3 = A(OUTPUT AREA)                                    
*                                                                               
         USING TACOD,R2                                                         
EXPIRET  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         ST    R4,FULL             SAVE ADDRESS OF OUTPUT AREA                  
         L     R5,ACOMFACS         NEEDED FOR ADDRESS OF PERVERT                
         USING COMFACSD,R5                                                      
         MVC   WORK,SPACES         CLEAR WORK AREA                              
         LA    R4,TACOFCYC         1ST FIXED CYCLE DATE                         
*                                                                               
EX10     MVC   WORK+9(5),=C'(21M)' DEFAULT OF 21 MONTHS                         
*                                                                               
         CLI   TACOTYPE,CTYSEAS2   IF SEASONAL COMMERCIAL                       
         BE    EX30                NEVER CALCULATE EXPIRATION DATE              
*                                                                               
EX12     CLI   TACOLEN,TACOLNQ2                                                 
         BL    EX12A                                                            
         TM    TACOSTA3,TACOSSMW   IF COMM HAS SOCIAL MEDIA WAIVER              
         BO    EX12B                                                            
EX12A    CLI   TACOMED,TACOMEDC    OR IF COMM'L MEDIA IS CABLE                  
         BE    *+12                                                             
         CLI   TACOTYPE,CTYPUB     OR IF COMM'L TYPE B                          
         BNE   *+10                                                             
EX12B    MVC   WORK+9(5),=C'(12M)' DEFAULT OF 12 MONTHS                         
*                                                                               
         TM    TACOSTAT,TACOSCRT   IF CANADIAN RATES                            
         BZ    EX14                                                             
         MVC   WORK+9(5),=C'(65W)' DEFAULT OF 65 WEEKS                          
*                                                                               
         CLI   TACOMED,TACOMEDT    IF CANADIAN TELEVISION                       
         BE    EX13                                                             
         CLI   TACOMED,TACOMEDI    OR INTERNET                                  
         BE    EX13                                                             
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         BNE   EX15                                                             
EX13     MVC   WORK+9(5),=C'(24M)' ACT DEFAULT OF 24 MONTHS                     
*                                                                               
         OC    TACOAIR,TACOAIR     IF THERE'S A 1ST AIR DATE                    
         BZ    EX15                                                             
         GOTO1 DATCON,DMCB,(1,(R4)),(0,BLOCK) CONVERT DATE TO EBCDIC            
         GOTO1 (RF),(R1),(1,TACOAIR),(0,BLOCK+6)                                
         GOTO1 CPERVERT,DMCB,BLOCK,BLOCK+6   L'TIME BETWEEN 1ST FIXED           
*                                              CYC AND 1ST AIR DATE             
         CLC   =H'7',DMCB+14       IF 1ST AIR W/IN 6 MTHS OF 1ST SERV           
         BL    EX15                                                             
         BH    *+14                                                             
         CLC   TACOAIR+2(1),2(R4)                                               
         BH    EX15                                                             
         MVC   WORK+9(5),=C'(18M)' THEN ONLY 18 MONTHS                          
         LA    R4,TACOAIR          AND USE 1ST AIR DATE                         
         B     EX15                                                             
*                                                                               
EX14     CLI   TACOTYPE,CTYICAT2   INDUSTRIALS CATEGORY 2                       
         BE    EX14A                                                            
         CLI   TACOTYPE,CTYICAT1   INDUSTRIALS CATEGORY 1                       
         BE    EX30                NO EXPIRY FOR CATEGORY 1                     
         BNE   EX15                                                             
*X14A    MVC   WORK+9(5),=C'(60M)' DEFAULT OF 5 YEARS                           
EX14A    MVC   WORK+9(5),=C'(36M)' DEFAULT OF 3 YEARS                           
         LA    R4,TACOFCYC         1ST FIXED CYCLE DATE                         
         B     EX15                                                             
*                                                                               
EX15     LTR   R3,R3               IF A BASIS WAS PASSED                        
         BZ    EX17                   USE THAT DATE                             
         LR    R4,R3                                                            
*                                                                               
EX17     GOTO1 DATCON,DMCB,(1,(R4)),(8,WORK)                                    
         MVI   WORK+8,C'-'         SET HYPHEN                                   
         LA    R4,BLOCK                                                         
         USING PERVALD,R4                                                       
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',(R4))                          
*                                                                               
EX20     L     R3,FULL             RETURN EXPIRATION DATE                       
         MVC   0(3,R3),PVALPEND                                                 
EX30     XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO MATCH CORRECT COMMENT RECORD TYPE IN RECTAB           
*                                                                               
GETCOM   NTR1  BASE=*,LABEL*                                                    
         USING RECD,R5                                                          
         LA    R3,COMTAB           COMMENT TYPE TABLE                           
*                                                                               
         LA    RF,8(R2)                                                         
         TM    TGBYTE,X'80'                                                     
         BZ    GETCOM5                                                          
         LR    RF,R2                                                            
*                                                                               
GETCOM5  CLC   0(1,R3),0(RF)       TYPE IS ON THE SCREEN                        
         BE    GETCOMX                                                          
         LA    R3,1(R3)            BUMP TO NEXT COMMENT TYPE                    
         CLI   0(R3),X'FF'                                                      
         BE    RVALERR6                                                         
         ZIC   RE,RECLEN           BUMP TO NEXT RECORD IN TABLE                 
         AR    R5,RE                                                            
         CLI   0(R5),X'FF'                                                      
         BE    RVALERR6                                                         
         CLC   RECCD,KEYSAVE       RECORD CODE SHOULD STILL MATCH               
         BNE   RVALERR6                                                         
         B     GETCOM5                                                          
         DROP  R5                                                               
YES3     SR    RC,RC                                                            
NO3      LTR   RC,RC                                                            
GETCOMX  XIT1  REGS=(R5)                                                        
*                                                                               
*              COMMENT TYPE TABLE                                               
*                                                                               
COMTAB   DC    C'G'                GUARANTEE                                    
         DC    C'C'                COMMERCIAL                                   
         DC    C'A'                CONTRACT (AGREEMENT)                         
         DC    C'I'                INVOICE                                      
         DC    C'V'                ADVICE (DETAILS)                             
         DC    C'F'                FINAL CAST LIST                              
         DC    X'FF'                                                            
*                                                                               
RVALERR6 MVI   ERROR,NOTFOUND      SET RECORD NOT FOUND ERROR                   
         TM    TGBYTE,X'04'        DOES USER WANT ERRORS RETURNED               
         BNZ   NO3                                                              
         GOTO1 ERREX                                                            
         EJECT                                                                  
*              ROUTINE TO MAKE TGDUC DISPLAYABLE USING WORK AND                 
*              DISPLAY TO SCREEN                                                
*                                                                               
DISPDUC  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGDUC,X'FA'         IS THIS A YEAR 2000 DATE?                    
         BL    DSPDUCX             NO                                           
         MVC   WORK(4),TGDUC       YES, CONVERT TO DISPLAY                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   8(4,R2),WORK        MOVE TO SCREEN                               
DSPDUCX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* UPDATE AGENCY RECORD WITH NEXT ADVICE CODE                                    
*---------------------------------------------------------------------          
         USING TAAYD,R3                                                         
UPDADV   NTR1  BASE=*,LABEL=*                                                   
         PACK  TGDUB,TAAYNADV+2(4)   UPDATE NEXT ADVICE CODE                    
         CVB   RF,TGDUB                                                         
         LA    RF,1(RF)                                                         
         CVD   RF,TGDUB                                                         
         OI    TGDUB+7,X'0F'                                                    
         UNPK  TAAYNADV+2(4),TGDUB                                              
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* SEE IF FULL NAME WILL FIT, OTHERWISE USE MIDDLE INITIAL                       
*---------------------------------------------------------------------          
FITNAME  NTR1  BASE=*,LABEL=*                                                   
         USING TAW4D,R3                                                         
         MVC   WORK+2(16),TAW4NAM2 LAST NAME                                    
         MVC   0(16,R4),TAW4NAM1   FIRST NAME                                   
         CLI   TAW4LEN,TAW4LN2Q                                                 
         BL    FITNAMEX                                                         
         MVC   17(16,R4),TAW4MIDN  MIDDLE NAME                                  
         GOTO1 SQUASHER,DMCB,WORK+2,50                                          
         CLI   DMCB+7,33                                                        
         BNH   FITNAMEX                                                         
         MVC   WORK+2(16),TAW4NAM2 LAST NAME                                    
         MVI   WORK+18,C' '                                                     
         MVC   0(16,R4),TAW4NAM1   FIRST NAME                                   
         MVI   16(R4),C' '                                                      
         MVC   17(16,R4),BSPACES                                                
         MVC   17(1,R4),TAW4MIDN   MIDDLE NAME                                  
         GOTO1 SQUASHER,DMCB,WORK+2,50                                          
         B     FITNAMEX                                                         
         DROP  R3                                                               
*                                                                               
BSPACES  DC    CL16' '                                                          
*                                                                               
FITNAMEX XIT1                                                                   
         EJECT                                                                  
*              ROUTINE BUILDS LIST OF AFM LOCALS WITH H&W FUNDS                 
         SPACE 1                                                                
BLDHWLCL NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TGHWLCLS         R2=A(NEXT SLOT IN TABLE)                     
         LA    R0,TGNLCLS          R0=MAX N'LOCALS                              
         MVI   ELCODE,TALOELQ      WILL BE LOOKING FOR LOCAL ELEMENT            
         SPACE 1                                                                
         LA    R4,KEY              BUILD KEY                                    
         XC    KEY,KEY                                                          
         USING TLLOD,R4                                                         
         MVI   TLLOCD,TLLOCDQ      RECORD EQUATE                                
         MVC   TLLOUN,=C'AFM'      SET UNION=AFM                                
         GOTO1 HIGH                                                             
         B     BLCL4                                                            
         SPACE 1                                                                
BLCL2    GOTO1 SEQ                                                              
         SPACE 1                                                                
BLCL4    CLC   TLLOKEY(TLLOLCL-TLLOD),KEYSAVE  INSURE UNION STILL AFM           
         BNE   XIT3                                                             
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         BAS   RE,GETEL3           GET LOCAL ELEMENT                            
         BNE   BLCL2                                                            
         USING TALOD,R3                                                         
         TM    TALOSTAT,TALOSHNW   TEST LOCAL HAS H&W FUND                      
         BZ    BLCL2                                                            
         LTR   R0,R0               TEST THERE'S ROOM FOR MORE                   
         BZ    NOROOMHW                                                         
         MVC   0(3,R2),TLLOLCL     SAVE THIS LOCAL                              
         LA    R2,3(R2)            BUMP TO NEXT SPOT IN TABLE                   
         AHI   R0,-1               REDUCE N'REMAINING                           
         B     BLCL2                                                            
         EJECT                                                                  
XIT3     XIT1                                                                   
         SPACE 1                                                                
         GETELN (R3),DATADISP,ELCODE,3                                          
         SPACE 1                                                                
NOROOMHW MVI   ERROR,ERNORMHW      NO MORE ROOM FOR H&W LOCALS                  
         LA    R2,CONSERVH         CURSOR TO SERVICE REQUEST FIELD              
         GOTO1 ERREX                                                            
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              SSNUNPK - DECRYPTS PID                                           
*                                  P1=A(PID)                                    
*                                  P2=A(SSN)                                    
*                                    CC=EQ GOOD DECRYPTION                      
*                                    CC=NEQ BAD DECRYPTION                      
*=====================================================================          
SSNUPK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,4(R1)            CLEAR OUT SS#, IN CASE PID INVALID           
         MVC   0(9,R3),SPACES                                                   
*                                                                               
         L     R2,0(R1)            PID #                                        
         LA    R3,6                6 DIGITS                                     
         LA    RF,BASE36TB                                                      
         XR    R0,R0                                                            
SSNUP10  CLI   0(RF),X'FF'         EOT?                                         
         BNE   SSNUP20                                                          
         LTR   RB,RB                                                            
         B     SSNUPXT             LEAVE, WITH SS# AS BLANK                     
SSNUP20  BAS   RE,SSNUCALC         UNPACK CALCULATIONS                          
         BNE   SSNUPXT                                                          
         A     R0,FULL                                                          
         LA    RF,L'BASE36TB(RF)                                                
         LA    R2,1(R2)                                                         
         BCT   R3,SSNUP10                                                       
         L     R3,4(R1)                                                         
         EDIT  (R0),(9,(R3)),ZERO=NOBLANK,FILL=0                                
         CR    RB,RB                                                            
SSNUPXT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
SSNUCALC NTR1                                                                   
         LA    R3,BASE36T2                                                      
SCALU10  CLI   0(R3),X'FF'                                                      
         BE    SCALUNO                                                          
         CLC   0(1,R2),1(R3)       FIND MATCH OF BASE 36 NUM IN TABLE           
         BE    SCALU20                                                          
         LA    R3,L'BASE36T2(R3)                                                
         B     SCALU10                                                          
SCALU20  XC    HALF,HALF                                                        
         MVC   HALF+1(1),0(R3)     HEX EQUIVALENT NUMBER                        
         ZICM  RE,0(RF),4                                                       
         MH    RE,HALF                                                          
         STCM  RE,15,FULL                                                       
*                                                                               
SCALUYES CR    RB,RB                                                            
         B     *+6                                                              
SCALUNO  LTR   RB,RB                                                            
         B     SSNUPXT                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TASS2PID                                                       
***********************************************************************         
*        ROUTINE TO ADD ERROR DETAILS TO ERROR TABLE                  *         
*        ON ENTRY ... P1=A(ERROR ENTRY TO ADD)                        *         
***********************************************************************         
                                                                                
AERROR   NTR1  BASE=*,LABEL=*                                                   
         USING ERRENTD,R3                                                       
         L     R3,0(R1)            R1=A(ERROR ENTRY)                            
         L     R4,TGAERTAB         R4=A(ERROR TABLE)                            
                                                                                
         CLI   EECATY,ERRCATY2     IF ERROR IS A CATEGORY 2                     
         JNE   AERR20                                                           
         CLI   TGAFAOER,0          AND ERRORS ARE BEING BYPASSED                
         JE    AERR20                                                           
         L     RF,TGAFAOER                                                      
         ZIC   RE,TGAFAOER                                                      
AERR10   CLC   EENUMB,0(RF)        IS THIS ERROR BEING BYPASSED?                
         JE    YES                 YES - DON'T ADD IT TO TABLE AND EXIT         
         LA    RF,2(RF)                                                         
         BCT   RE,AERR10                                                        
                                                                                
AERR20   CLI   EECATY,ERRCATY3     IF THIS ERROR IS A CATEGORY 3                
         JE    AERR40              ADD IT TO BEGINNING OF TABLE                 
AERR30   CLI   0(R4),X'FF'                                                      
         JE    AERR40              ELSE, FIND NEXT OPEN SLOT                    
         ZIC   RE,0(R4)            IN ERROR TABLE AND ADD IT THERE              
         AR    R4,RE                                                            
         J     AERR30                                                           
AERR40   ZIC   R0,EELEN                                                         
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),ERRENTD                                                  
                                                                                
         AR    R4,R0                                                            
         MVI   0(R4),X'FF'         MARK NEW END OF ERROR TABLE                  
                                                                                
         CLI   EECATY,ERRCATY3     IF ERROR IS A CATEGORY 3                     
         JNE   YES                 APPLICATION SHOULD TERMINATE NOW             
         J     NO                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE COUNTRY CODE                             *         
*        ON ENTRY ... P1, BYTE 0 = X'80' (NON-SCREEN FIELD)           *         
*                     P1 = A(NON-SCREEN FIELD OR SCREEN FIELD HEADER) *         
***********************************************************************         
                                                                                
                                                                                
VCTRY    NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(COUNTRY FIELD HEADER)                   
         LR    R3,R2                                                            
                                                                                
         TM    0(R1),X'80'         IF R2=A(SCREEN FIELD HEADER)                 
         JO    VCTRY10                                                          
         LA    R3,8(R2)            R3=A(SCREEN FIELD)                           
         CLI   5(R2),0             AND INPUT IS REQUIRED                        
         JNE   VCTRY10                                                          
         MVI   ERROR,MISSING                                                    
         J     NO                                                               
                                                                                
         USING CTRYTABD,R1                                                      
VCTRY10  L     R1,TGACTRYS                                                      
VCTRY20  CLC   CTRYCODE,0(R3)      INPUT MUST BE A VALID COUNTRY CODE           
         JE    VCTRY30                                                          
         ZIC   R0,CTRYLEN                                                       
         AR    R1,R0                                                            
         CLI   0(R1),X'FF'                                                      
         JNE   VCTRY20                                                          
         MVI   ERROR,INVALID                                                    
         J     NO                                                               
                                                                                
VCTRY30  ST    R1,TGACTRY          SAVE A(COUNTRY TABLE ENTRY)                  
         MVC   TGCTRY,CTRYCODE     AND SAVE GLOBAL COUNTRY CODE                 
         J     YES                                                              
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO TRANSLATE AGENT CODE                              *         
*        INCLUDED BY TASYSVAL AND TALNK01                             *         
*        ON ENTRY ... P1 BYTE 0 = X'80' CHARACTER TO NUMERIC          *         
*                                 X'40' NUMERIC TO CHARACTER          *         
*                     P1        = A(INCOMING AGENT CODE)              *         
*                     P2        = A(TRANSLATED AGENT CODE)            *         
***********************************************************************         
                                                                                
TAGT     NTR1  BASE=*,LABEL=*                                                   
       ++INCLUDE TATRNSAGT                                                      
***********************************************************************         
*        ROUTINE COPIES NEW-STYLE OVERSCALE 2ND PERCENT               *         
*        INTO CAST DETAILS ELEMENT                                    *         
*        ON ENTRY ... P1 = A(CAST DETAILS ELEMENT)                    *         
*                     P2 = A(CAST RECORD)                             *         
*                     P3 = A(USE TYPE)                                *         
***********************************************************************         
                                                                                
SOV2     NTR1  BASE=*,LABEL=*                                                   
         USING TACAD,R2                                                         
         LM    R2,R4,0(R1)                                                      
                                                                                
         GOTO1 HELLO,DMCB,(C'G',=CL8'TALFIL'),('TAO2ELQ',(R3))                  
         CLI   DMCB+12,0                                                        
         JNE   XIT                 IF CAST HAS OVERSCALE 2ND % ELEMENT          
                                                                                
         USING USETABD,R5                                                       
         L     R5,TGAUSES                                                       
SOV210   CLC   USECDE,0(R4)        LOOK UP USE                                  
         JE    SOV220                                                           
         LH    RE,USELEN                                                        
         AR    R5,RE                                                            
         CLI   0(R5),X'FF'                                                      
         JNE   SOV210                                                           
                                                                                
         USING TAO2D,R3                                                         
SOV220   L     R3,DMCB+12                                                       
         ZIC   R0,TAO2NUM                                                       
         LA    R3,TAO2SBEL         R4=A(1ST 2ND OVERSCALE % SUB-ELEM)           
         DROP  R3                                                               
         USING TAO2SBEL,R3                                                      
                                                                                
SOV230   CLC   TAO2USE,0(R4)       IF USE TYPE IS FOUND                         
         JNE   SOV240                                                           
         MVC   TACAOV2,TAO2PCT     SET 2ND OVERSCALE PERCENT                    
         J     XIT                 AND EXIT                                     
                                                                                
SOV240   CLC   TAO2USE,=C'ARE'     APPLIES TO ALL REUSE                         
         JNE   SOV250                                                           
         TM    USESTAT,SESSION     AND USE IS REUSE                             
         JO    SOV250                                                           
         MVC   TACAOV2,TAO2PCT     SET PCT OVSCL (& CONTINUE LOOKING)           
         DROP  R5                                                               
                                                                                
SOV250   CLC   TAO2USE,SPACES      IF 2ND OVERSCALE PERCENT APPLIES             
         JNE   SOV260              TO ALL USE, SET 2ND OVERSCALE                
         OC    TACAOV2,TACAOV2     AND PCT NOT ALREADY SET                      
         JNZ   SOV260                                                           
         MVC   TACAOV2,TAO2PCT     PERCENT AND CONTINUE LOOKING                 
                                                                                
SOV260   LA    R3,L'TAO2SBEL(R3)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R0,SOV230                                                        
         J     XIT                                                              
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO CHECK IF LOCKET ACTIVE                            *         
*            0(R1) = A(LOCKET KEY)                                    *         
*        RETURN CC .. EQUAL (CHECK LOCKOUT ACTIVE)                    *         
*                     NOT EQUAL NOT ACTIVE                            *         
***********************************************************************         
         USING LKKEYD,RF                                                        
TSTLCKTA NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'                                                     
         JE    NO                                                               
         LA    RF,WORK                                                          
         L     RE,0(R1)                                                         
         XC    LOCKEY,LOCKEY                                                    
         MVC   LOCKSE,SVSYS                                                     
         MVC   LOCKKEY,0(RE)       USE LOCKET KEY THAT WAS PASSED               
         DROP  RF                                                               
                                                                                
TSTLCKT5 L     RF,ACOMFACS                              TEST LOCK SET           
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',WORK),ACOMFACS                              
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    TSTLCKT5            YES - TRY AGAIN                              
         CLI   4(R1),1             TEST ALREADY LOCKED                          
         JE    YES                 YES - CC=EQUAL                               
         J     NO                  NO  - CC=NEQ                                 
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        ROUTINE TO CLEAR KEY IN LOCKET                               *         
*            0(R1) = A(LOCKET KEY)                                    *         
*            7(R1) = 0, CLEAR KEY                                     *         
*                  = 1, SET KEY                                                 
*        RETURN CC .. EQUAL (CHECK LOCKOUT ACTIVE)                    *         
*                     NOT EQUAL NOT ACTIVE                            *         
***********************************************************************         
         USING LKKEYD,RF                                                        
SETLCKTA NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'                                                     
         JE    YES                                                              
         LA    RF,WORK                                                          
         L     RE,0(R1)                                                         
         XC    LOCKEY,LOCKEY                                                    
         MVC   LOCKSE,SVSYS                                                     
         MVC   LOCKKEY,0(RE)       USE LOCKET KEY THAT WAS PASSED               
         DROP  RF                                                               
                                                                                
SETLCKT5 L     RF,ACOMFACS                              TEST LOCK SET           
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         MVI   BYTE,LKUNLKQ        UNLOCK                                       
         CLI   7(R1),0                                                          
         JE    *+8                                                              
         MVI   BYTE,LKLOCKQ        LOCK                                         
*                                                                               
         GOTO1 (RF),DMCB,(BYTE,WORK),ACOMFACS                                   
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    SETLCKT5            YES - TRY AGAIN                              
         CLI   4(R1),0             ANY ERRORS                                   
         JE    YES                 NO  - CC=EQUAL                               
         J     NO                  YES - CC=NEQ                                 
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAVALTYP                                                       
***********************************************************************         
*              ROUTINE TO TEST 4-BYTE UNION AREA AGAINST EQUATES                
*                                  P1=A(4-BYTE BLOCK OF UNION BYTES             
*                                       TO TEST - TGUNEQUS, TGCAUNIS            
*                                       OR TGUSXUNS)                            
*                                     BYTE 0 X'80'=TEST THIS BLOCK              
*                                                  AGAINST A BLOCK OF           
*                                                  STORAGE INSTEAD OF           
*                                                  STRAIGHT EQUATES             
*                                  P2=UNION EQUATES TO TEST FIRST               
*                                     BYTE FOR                                  
*                                     OR, IF BYTE 0 OF P1 IS X'80',             
*                                     A(4-BYTE BLOCK OF UNION BYTES             
*                                       TO TEST AGAINST 4-BYTE BLOCK            
*                                       OF UNION BYTES SPECIFIED BY             
*                                       P1)                                     
*                                  P3=UNION EQUATES TO TEST SECOND              
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*                                  P4=UNION EQUATES TO TEST THIRD               
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*                                  P5=UNION EQUATES TO TEST FOURTH              
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*              RETURN CONDITION CODE                                            
***********************************************************************         
                                                                                
UTEST    NTR1  BASE=*,LABEL=*                                                   
       ++INCLUDE TAUNITEST                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
*        "MOVE TO INTERNET" AND "MOVE TO NEW MEDIA" USES NEED TO      *         
*        SEARCH FOR SESSION OVERSCALES TOO                            *         
***********************************************************************         
                                                                                
GTOVMV   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'MVI',0(R2)       IF OVERSCALE AMOUNT IS NOT FOUND             
         JE    GOM10               AND USE IS MOVED TO INTERNET                 
         CLC   =C'MVN',0(R2)       OR MOVED TO NEW MEDIA                        
         JE    GOM10                                                            
         CLC   =C'SMI',0(R2)       OR SPANISH MOVED TO INTERNET                 
         JE    GOM10                                                            
         CLC   =C'SMN',0(R2)       OR SPANISH MOVED TO NEW MEDIA                
         JNE   XIT                                                              
*                                                                               
         USING TAOPD,R3                                                         
GOM10    L     R3,AIO                                                           
         MVI   ELCODE,TAOPELQ      FIRST LOOK FOR OVERSCALE PERCENT             
         BRAS  RE,GETEL                                                         
         JNE   GOM60                                                            
         ZIC   R0,TAOPNUM          R0=N'SUB-ELEMENTS                            
*                                                                               
         LA    RE,TAOPSBEL                                                      
         USING TAOPSBEL,RE         RE=A(FIRST SUB-ELEMENT)                      
*                                                                               
GOM20    LA    RF,GOSESTAB         RF=A(TABLE OF SESSION USES)                  
GOM30    CLI   0(RF),X'FF'                                                      
         JE    GOM50                                                            
         CLC   TAOPUSE,0(RF)       IF SESSION OVERSCALE IS FOUND                
         JE    GOM40                                                            
         LA    RF,3(RF)                                                         
         J     GOM30                                                            
GOM40    MVC   0(4,R4),TAOPPCT     SET PERCENT OVERSCALE                        
         J     GOM60                                                            
*                                                                               
GOM50    LA    RE,L'TAOPSBEL(RE)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R0,GOM20                                                         
*                                                                               
GOM60    TM    BYTE,X'80'          IF ONLY PERCENT REQUIRED                     
         JO    XIT                 GET OUT NOW                                  
*                                                                               
         USING TAOAD,R3                                                         
GOM70    L     R3,AIO                                                           
         MVI   ELCODE,TAOAELQ      SET TO READ THROUGH OVERSCALE                
         BRAS  RE,GETEL            AMOUNTS AGAIN                                
         JNE   XIT                                                              
         ZIC   R0,TAOANUM          R0=N'SUB-ELEMENTS                            
*                                                                               
         LA    RE,TAOASBEL         RE=A(FIRST SUB-ELEMENT)                      
         USING TAOASBEL,RE                                                      
*                                                                               
GOM80    LA    RF,GOSESTAB         RF=A(TABLE OF SESSION USES)                  
GOM90    CLI   0(RF),X'FF'                                                      
         JE    GOM120                                                           
         CLC   TAOAUSE,0(RF)       IF SESSION OVERSCALE IS FOUND                
         JE    GOM100                                                           
         LA    RF,3(RF)                                                         
         J     GOM90                                                            
GOM100   ZICM  RF,TAOAAMT,4        USE IT FOR  INITIAL 8-WEEK                   
         CLI   TGUSTYP,UMVII8W     ELSE MULTIPLY IT BY 3                        
         BE    GOM110                                                           
         M     RE,=F'3'                                                         
GOM110   STCM  RF,15,0(R4)         AND SET AMOUNT                               
         MVI   0(R1),X'FF'         SET WE HAVE AMOUNT                           
         J     XIT                                                              
*                                                                               
GOM120   LA    RE,L'TAOASBEL(RE)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R0,GOM80                                                         
         J     XIT                                                              
         DROP  RE                                                               
*                                                                               
GOSESTAB DC    C'BSS'                                                           
         DC    C'SSS'                                                           
         DC    C'SCS'                                                           
         DC    C'BSC'                                                           
         DC    C'BSU'                                                           
         DC    C'DEM'                                                           
         DC    C'ADD'                                                           
         DC    C'SNA'                                                           
         DC    C'CDM'                                                           
         DC    C'CAU'                                                           
         DC    C'RRS'                                                           
         DC    C'ARS'                                                           
         DC    C'SRS'                                                           
         DC    C'ADC'                                                           
         DC    C'BSM'                                                           
         DC    C'IMS'                                                           
         DC    C'CMS'                                                           
         DC    C'ISS'                                                           
         DC    C'PRM'                                                           
         DC    C'IFS'                                                           
         DC    C'TAG'                                                           
         DC    C'VAR'                                                           
         DC    C'ADT'                                                           
         DC    C'ADO'                                                           
         DC    C'AUD'                                                           
         DC    C'NBS'                                                           
         DC    C'CNL'                                                           
         DC    C'SCN'                                                           
         DC    C'INS'                                                           
         DC    C'FGS'                                                           
         DC    C'SFS'                                                           
         DC    C'PUB'                                                           
         DC    C'SOP'                                                           
         DC    C'RRR'                                                           
         DC    C'ARR'                                                           
         DC    C'BSR'                                                           
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
*        ROUTINE TO BUILD STAFF'S LIMITED AGENCY/CLIENT LIST          *         
***********************************************************************         
                                                                                
BLDS2LIM NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'                                                     
         JE    XIT                                                              
*                                                                               
         USING BS2LD,R3                                                         
         LA    R3,BLOCK                                                         
         MVC   BS2LSDIR,SYSDIR     SAVE DIRECTORY                               
         MVC   BS2LSFIL,SYSFIL     AND FILE SETTINGS                            
*                                                                               
         MVC   BS2LSKEY,KEY        SAVE KEY                                     
         L     RE,AIO                                                           
         MVC   BS2LSAIO(32),0(RE)  SAVE KEY OF RECORD IN AIO                    
*                                                                               
***********************************************************************         
                                                                                
         MVC   SYSDIR,=CL8'TALDIR' SET TO READ TALENT FILE                      
         MVC   SYSFIL,=CL8'TALFIL'                                              
*                                                                               
         USING TLSTD,R1                                                         
         LA    R1,KEY                                                           
         XC    KEY,KEY             FIRST LOOK FOR STAFF RECORD                  
         MVI   TLSTCD,TLSTCDQ      WITHOUT A USER ID                            
         MVC   TLSTSTAF,TGCTSTAF                                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLSTSSEQ-TLSTD),KEYSAVE                                      
         JE    BLIM10                                                           
*                                  IF NOT FOUND, LOOK FOR STAFF                 
         MVC   KEY,KEYSAVE         RECORD WITH USER ID                          
         MVC   TLSTUSER,TWAORIG    (IF IF DOES NOT EXIST, STAFF                 
         GOTO1 HIGH                 HAS NO ACCESS)                              
         CLC   KEY(TLSTSSEQ-TLSTD),KEYSAVE                                      
         JNE   BLIM70                                                           
         DROP  R1                                                               
*                                                                               
BLIM10   MVI   BS2LSSEQ,1          SET TO ADD 1ST WSSVR BLOCK                   
         J     BLIM30                                                           
*                                                                               
BLIM20   GOTO1 SEQ                 READ REST OF THE STAFF KEYS                  
         CLC   KEY(TLSTSSEQ-TLSTD),KEYSAVE                                      
         JNE   BLIM70                                                           
*                                                                               
BLIM30   GOTO1 GETREC                                                           
*                                                                               
         USING TLRCD,R2                                                         
         L     R2,AIO              BUMP TO FIRST ELEMENT                        
         LA    R2,TLRCELEM                                                      
         DROP  R2                                                               
*                                                                               
         LR    RE,R2                                                            
BLIM40   CLI   0(RE),0             SET ALL NON-TAVA ELEMENTS                    
         JE    BLIM60              FOR DELETION                                 
         CLI   0(RE),TAVAELQ                                                    
         JE    BLIM50                                                           
         MVI   0(RE),X'FF'                                                      
BLIM50   ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         J     BLIM40                                                           
*                                                                               
BLIM60   MVI   ELCODE,X'FF'        DELETE ALL NON-TAVA ELEMENTS                 
         GOTO1 REMELEM                                                          
*                                                                               
***********************************************************************         
                                                                                
         USING COMFACSD,RF                                                      
         L     RF,ACOMFACS                                                      
*                                                                               
         USING FAWSSVRD,R1                                                      
         LA    R1,BS2LWBLK                                                      
         MVC   FAWSTOKN(3),=CL3'STF'                                            
         MVC   FAWSTOKN+3(1),BS2LSSEQ                                           
         MVI   FAWSACTN,FAWSASVE                                                
         LHI   R0,STF2LNQ                                                       
         STH   R0,FAWSLEN          SAVE STAFF RECORD INTO WSSVR AREA            
         ST    R2,FAWSADR                                                       
         GOTO1 CWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1,RF                                                            
*                                                                               
***********************************************************************         
                                                                                
         ZIC   RE,BS2LSSEQ         SET TO ADD NEXT WSSVR BLOCK                  
         AHI   RE,1                                                             
         STC   RE,BS2LSSEQ                                                      
         J     BLIM20              AND GO READ NEXT STAFF RECORD                
*                                                                               
***********************************************************************         
                                                                                
BLIM70   OC    BS2LSAIO,BS2LSAIO   IF AIO AREA WAS POPULATED                    
         JZ    BLIM90                                                           
         MVC   KEY,BS2LSAIO        RESTORE IT                                   
         BAS   RE,BLSTFILE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         JNE   BLIM80                                                           
         GOTO1 GETREC                                                           
BLIM80   MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
*                                                                               
BLIM90   OC    BS2LSKEY,BS2LSKEY   IF KEY WAS POPULATED                         
         JZ    BLIM100                                                          
         MVC   KEY,BS2LSKEY        RESTORE KEY                                  
         BAS   RE,BLSTFILE                                                      
         GOTO1 HIGH                AND THE READ SEQUENCE                        
*                                                                               
BLIM100  MVC   SYSDIR,BS2LSDIR     RESTORE DIRECTORY                            
         MVC   SYSFIL,BS2LSFIL     AND FILE SETTINGS                            
         J     XIT                                                              
         DROP  R3                                                               
*                                                                               
BLSTFILE LA    RF,CHKRECS                                                       
BLSF10   CLI   0(RF),X'FF'                                                      
         BER   RE                                                               
         CLC   KEY(1),0(RF)                                                     
         BE    BLSF20                                                           
         LA    RF,1(RF)                                                         
         B     BLSF10                                                           
BLSF20   MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         BR    RE                                                               
*                                                                               
CHKRECS  DC    AL1(TLDTCDQ)                                                     
         DC    AL1(TLCKCDQ)                                                     
         DC    AL1(TLCKHCDQ)                                                    
         DC    AL1(TLCKDCDQ)                                                    
         DC    AL1(TLCKLCDQ)                                                    
         DC    AL1(TLCKCCDQ)                                                    
         DC    AL1(TLCKECDQ)                                                    
         DC    AL1(TLCKYCDQ)                                                    
         DC    AL1(TLCKCSSQ)                                                    
         DC    AL1(TLCKCSCQ)                                                    
         DC    AL1(TLCKKCDQ)                                                    
         DC    AL1(TLCKCPSQ)                                                    
         DC    AL1(TLCKCPCQ)                                                    
         DC    AL1(TLCKBCDQ)                                                    
         DC    AL1(TLW2CDQ)                                                     
         DC    X'FF'                                                            
*                                                                               
STF2LNQ  EQU   (2000-(TLRCELEM-TLRCD))                                          
         EJECT                                                                  
***********************************************************************         
*        LOCAL ROUTINE TO CHECK FOR LIMIT ACCESS RESTRICTIONS         *         
***********************************************************************         
                                                                                
LIMCHK   NTR1  BASE=*,LABEL=*                                                   
         CLI   KEY,TLAYCDQ         IF VALIDATING AGENCY RECORD                  
         JNE   LIMCHK10                                                         
         CLC   TGAGY,=C'999999'    THEN DON'T ALLOW '999999'                    
         JNE   LIMCHK20                                                         
         TM    TGBYTE,X'01'        UNLESS BIT SET TO ALLOW IT                   
         JO    LIMCHK20                                                         
         CLI   OFFLINE,C'Y'        OR OFFLINE                                   
         JE    LIMCHK20                                                         
         CLI   TGCTSTTY,TASTTYPP   OR PROGRAMMER                                
         JE    LIMCHK20                                                         
         J     NO                                                               
*                                                                               
LIMCHK10 CLI   KEY,TLCLCDQ         IF VALIDATING CLIENT RECORD                  
         JNE   YES                 OR AGENCY RECORD                             
LIMCHK20 GOTOR CHKIT2,DMCB,TGCLI   CHECK AGAINST STAFF2 LIMITS                  
         J     XIT                 CC SET                                       
         EJECT                                                                  
***********************************************************************         
*        CHECK CLIENT LIMIT ACCESS IF READING COMM'LS VIA COMM'L ID   *         
*        PTR OR READING INVOICES THROUGH ACTIVE PTR                   *         
*        P1 = A(CLIENT CODE)                                          *         
***********************************************************************         
                                                                                
LIMCHK2  NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'        IF ON-LINE                                   
         JE    YES                                                              
         TM    TGBYTE3,X'80'       AND NOT REQUESTING TO SKIP LIMIT             
         JO    YES                                                              
         OC    TGCLGACC,TGCLGACC   AND NOT USING CLIENT GROUP ACCESS            
         JNZ   YES                                                              
*                                                                               
         L     R2,0(R1)            R2=A(CLIENT CODE)                            
         GOTOR CHKIT2,DMCB,(R2)                                                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO TEST STAFF2 LIMIT ACCESS                          *         
*        P1 = A(CLIENT CODE)                                          *         
***********************************************************************         
                                                                                
CHKIT2   NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'                                                     
         JE    YES                                                              
         TM    TGFASTAT,TGFROMFA                                                
         JO    YES                                                              
*                                                                               
         L     R3,0(R1)            R3=A(CLIENT CODE)                            
*                                                                               
         LHI   R2,1                                                             
*                                                                               
         USING COMFACSD,RF                                                      
CHKIT210 L     RF,ACOMFACS                                                      
*                                                                               
         USING FAWSSVRD,R1                                                      
         LA    R1,PARAS            TEMP STORAGE+2 BYTES OF DMCB                 
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 CWSSVR,(R1)                                                      
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         JNE   NO                                                               
         DROP  R1,RF                                                            
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         JZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
CHKIT220 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         JE    CHKIT210                                                         
*                                                                               
         CLC   TGAGY,TAVAAGY       IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   CHKIT240                                                         
         CLI   KEY,TLAYCDQ         AND CHECKING AGENCY RECORD                   
         JE    YES                 ACCESS IS GRANTED                            
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         JE    YES                 ACCESS IS GRANTED                            
         OC    0(L'TGCLI,R3),0(R3)                                              
         JZ    YES                                                              
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CHKIT230 CLC   0(L'TGCLI,R3),0(RF) IF CLIENT IS FOUND IN STAFF LIMITS           
         JE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CHKIT230                                                         
*                                                                               
CHKIT240 ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         J     CHKIT220                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE RADIO GUARANTEE TYPES                    *         
*        ON ENTRY ... R4=A(USE TYPE)                                            
***********************************************************************         
                                                                                
GRRTYPE  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UGRR        IF USE IS GRR                                
         JNE   NO                                                               
*                                                                               
         USING USETABD,R2                                                       
         L     R2,TGAUSES          R2=A(USE TABLE)                              
GRRTY10  CLI   0(R2),X'FF'         IF AT END OF TABLE, ERROR                    
         JE    NO                                                               
         CLC   USEEQU,0(R4)        TEST IF TYPE EXISTS AS USE                   
         JE    GRRTY20                                                          
         LH    RE,USELEN           BUMP TO NEXT USE ENTRY                       
         AR    R2,RE                                                            
         J     GRRTY10                                                          
*                                                                               
GRRTY20  TM    USEMEDS,RADIO       TYPE MUST BE VALID FOR RADIO                 
         JZ    NO                                                               
         MVC   TGUSTYP,USEEQU      SAVE USE TYPE EQUATE IN GLOBAL STOR.         
         MVI   TGUSTYMD,RADIO      VALID MEDIA FOR TYPE                         
         MVI   TGUSWKS,13          N'WEEKS IN CYCLE (X'80'==>MTHS)              
         MVC   TGUSTYCD,USECDE     TYPE CODE                                    
*                                                                               
         MVC   TGUSNAME,=CL16'RADIO GRT ('                                      
         MVC   TGUSNAME+11(3),USECDE                                            
         MVI   TGUSNAME+14,C')'                                                 
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE AN ACTRA COMMERCIAL TYPE                 *         
*        ON ENTRY ... BYTE  0   = X'80' = EQUATE                      *         
*                     BYTES 1-3 = A(4 BYTE COMML TYPE)                *         
***********************************************************************         
                                                                                
CCTYVAL  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            ACTRA COMMERCIAL TYPE                        
         L     R3,TGACTYPS                                                      
         USING CCTYPD,R3                                                        
                                                                                
CCTY10   CLI   0(R3),X'FF'                                                      
         JE    NO                                                               
         TM    0(R1),X'80'         IF TEST FOR EQUATE                           
         JZ    CCTY20                                                           
         CLC   CCTYPEQU,0(R2)                                                   
         JE    CCTY40                                                           
         J     CCTY30                                                           
                                                                                
CCTY20   CLC   CCTYPCDE,0(R2)                                                   
         JE    CCTY40                                                           
                                                                                
CCTY30   LA    R3,CCTYPNXT                                                      
         J     CCTY10                                                           
                                                                                
CCTY40   MVC   TGCCTCDE,0(R3)      CODE                                         
         MVC   TGCCTEQU,4(R3)      EQUATE                                       
         MVC   TGCCTNM,5(R3)       NAME                                         
         J     YES                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              ROUTINE CALCULATES NEXT BUSINESS DAY                             
*                 DMCB(0) HOLD DATE TO FIND NEXT BUSINESS DAY FROM              
*=====================================================================          
GTNXTBUS NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK           SET TO ADD 1 BUSINESS DAY TO TODAY           
         LA    R3,WORK             R3=A(GETRET BLOCK)                           
         USING GETRETD,R3                                                       
         GOTO1 DATCON,DMCB,,(3,GRDIDY)                                          
         MVC   GRDHRS,=H'24'                        + 24 HOURS                  
         SPACE 1                                                                
         L     RF,ACOMFACS         USE GETRET TO ADD BUSINESS DAYS              
         USING COMFACSD,RF                                                      
         OI    GRDFLAG,GRDFTAL                                                  
         GOTO1 CGETRET,(R3)        CALCULATE NEXT BUSINESS DAY                  
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS DUMMY NAME ELEMENT IN WORK                    *         
*        ON ENTRY ... AIO = A(W4 RECORD)                              *         
***********************************************************************         
                                                                                
BLDW4NME NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         L     R3,AIO                                                           
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TAW4D,R3                                                         
         MVC   WORK,SPACES                                                      
         MVI   WORK,TANAELQ        BUILD NAME ELEMENT IN WORK                   
         MVI   WORK+1,2+33                                                      
                                                                                
         LA    R4,WORK+19          SET TO SKIP A SPACE BETWEEN NAMES            
         CLI   TAW4TYPE,TAW4TYCO   FOR CORPORATIONS                             
         JE    *+20                                                             
         CLI   TAW4TYPE,TAW4TYTR   TRUSTEES                                     
         JE    *+12                                                             
         CLI   TAW4TYPE,TAW4TYES   AND ESTATES                                  
         JNE   *+6                                                              
         BCTR  R4,0                DON'T                                        
         DROP  R3                                                               
                                                                                
         BRAS  RE,FITNAME                                                       
                                                                                
         GOTO1 SQUASHER,DMCB,WORK+2,33  SQUASH NAMES TOGETHER                   
                                                                                
         MVI   ELCODE,TANAELQ      RESTORE ELCODE TO MATCH ELEMENT              
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE A MEDIA CODE                                 
*                                                                               
*                                  P1 = A(5-BYTE WORK AREA) OR A(EQU)           
*                                  P1 BYTE 0  X'80' = P1 IS EQUATE              
         SPACE 1                                                                
MVAL     NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2 = A(WORK AREA OR EQUATE)                  
         L     R3,TGAMEDS          R3 = A(MEDIA TABLE)                          
         USING MEDIAD,R3                                                        
                                                                                
MEV10    CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         JE    NO                                                               
                                                                                
         TM    0(R1),X'80'         IF TEST FOR EQUATE                           
         JZ    MEV20                                                            
         CLC   MEDEQU,0(R2)        THEN TEST FOR EQUATE                         
         JE    MEV50                                                            
         J     MEV30                                                            
                                                                                
MEV20    CLC   MEDNAME(1),0(R2)    ELSE TEST FOR NAME                           
         JE    MEV50                                                            
                                                                                
MEV30    LA    R3,MEDNEXT          BUMP R3 TO NEXT MEDIA CODE                   
         J     MEV10                                                            
                                                                                
MEV50    MVC   TGMEEQU,MEDEQU      SAVE EQUATE IN GLOBAL                        
         MVC   TGMENAME,MEDNAME    SAVE NAME IN GLOBAL                          
         J     YES                 RETURN EQ                                    
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER BLD2SLIM ROUTINE VARIABLES                        
*                                                                               
BS2LD    DSECT                                                                  
BS2LSDIR DS    XL(L'SYSDIR)        SAVED DIRECTORY SETTING                      
BS2LSFIL DS    XL(L'SYSFIL)        SAVED FILE SETTING                           
BS2LSKEY DS    XL(L'KEY)           SAVED KEY                                    
BS2LSAIO DS    XL(L'KEY)           SAVED AIO KEY                                
*                                                                               
BS2LSSEQ DS    XL1                 WSSVR BLOCK SEQUENCE NUMBER                  
BS2LWBLK DS    XL50                WSSVR BLOCK AREA                             
         EJECT                                                                  
*              DSECTS TO COVER RECTAB                                           
         SPACE 1                                                                
RECD     DSECT                                                                  
RECLEN   DS    AL1                 L'TABLE ENTRY                                
RECCD    DS    AL1                 RECORD CODE                                  
RECNFLDS DS    AL1                 N'FIELD ENTRIES                              
RECFLDS  DS    0C                  FIRST FIELD ENTRY                            
         SPACE 3                                                                
FLDD     DSECT                                                                  
FLDKDSP  DS    AL1                 DISP. TO FIELD IN KEY                        
FLDGDSP  DS    AL2                 DISP. TO FIELD IN GLOBAL STORAGE             
FLDLEN   DS    AL1                 L'FIELD-1                                    
FLDLNQ   EQU   *-FLDD                                                           
FLDNEXT  EQU   *                                                                
         EJECT                                                                  
SYSWORKD DSECT                                                                  
       ++INCLUDE TASYSWORKD                                                     
         EJECT                                                                  
*              DSECT TO COVER TWA                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         DS    CL64                                                             
CONHEADH DS    CL8                                                              
CONHEAD  DS    CL60                SYSTEM MESSAGE AREA                          
CONSERVH DS    CL8                                                              
CONSERV  DS    CL17                SERVICE REQUEST FIELD                        
CONHED2H DS    CL8                                                              
CONHED2  DS    CL79                PROGRAM MESSAGE AREA                         
CONHED2X DS    CL8                                                              
         SPACE 2                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* TAGENFILE                                                                     
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDTWADCOND                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDCOMFACS                                                                     
* DDGETRETD                                                                     
* DDPERVALD                                                                     
* DDMASTD                                                                       
* FASSB                                                                         
* FATIOB                                                                        
* TALDCPTRD                                                                     
* FAGETTXTD                                                                     
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAPROGSPCD                                                     
       ++INCLUDE TALDCPTRD                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TASYSVAL  10/30/19'                                      
         END                                                                    
