*          DATA SET TAGENED    AT LEVEL 079 AS OF 07/07/14                      
*PHASE T702EDA                                                                  
         TITLE 'T702ED - COMMENT MAINTENANCE'                                   
T702ED   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702ED                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         MVC   SVRETRN,SVRECUSE    SAVE REC/USE OF SCREEN TO RETURN             
         BAS   RE,CHKREC           CHECK RECORD AGAINST COMMENT TYPE            
         BRAS  RE,SETSCRN          SET UP SCREEN DEPENDING ON TYPE              
         BAS   RE,SETCMLEV         SET COMMENT LEVEL TP OR CLIENT               
         BAS   RE,CHGRETRN         CHANGE WHAT SCREEN TO RETURN TO              
         BAS   RE,SETPFTB          RETURN ADDRESS OF PFTABLE IN R3              
         GOTO1 INITIAL,DMCB,(X'40',(R3))                                        
         BAS   RE,SETPFK           SET PFKEY LINE                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
         SPACE 3                                                                
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    COM40                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    COM40                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   COM50                                                            
*                                                                               
COM40    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         BAS   RE,COMPPRO                                                       
         B     XIT                                                              
         SPACE 3                                                                
COM50    CLI   MODE,RECDEL         IF DELETING                                  
         BE    COM60                                                            
         CLI   MODE,VALREC         OR VALIDATING                                
         BNE   COM70                                                            
COM60    BAS   RE,VALWID           VALIDATE WEB APPLICATION ID                  
         SPACE 3                                                                
COM70    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   XIT                                                              
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*              VALIDATE THE KEY                                                 
*                                                                               
VK       MVC   KEY,SVKEY                                                        
         GOTO1 FLDVAL,DMCB,(X'40',COMAGYH),(X'80',COMENDH)                      
         BE    VKX                                                              
*                                                                               
         XC    SVWID,SVWID                                                      
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'60',COMAGYH)    AGENCY                    
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         LA    R2,COMTYPEH                                                      
         CLI   COMTYPE,TLCMTGUA    IF COMMENT TYPE IS GUARANTEE                 
         BE    ERRINV                                                           
*                                                                               
VK10     CLI   COMTYPE,TLCMTCOM    IF COMMENT TYPE IS COMMERCIAL                
         BNE   VK20                                                             
         BAS   RE,VALCOML          VALIDATE COMM ID                             
         B     VK30                                                             
*                                                                               
VK20     CLI   COMTYPE,TLCMTCON    IF COMMENT TYPE IS CONTRACT                  
         BNE   VK25                                                             
         BAS   RE,VALCON           VALIDATE CONTRACT ID AND DATES               
         B     VK30                                                             
*                                                                               
VK25     CLI   COMTYPE,TLCMTINV    IF COMMENT TYPE IS INVOICE                   
         BNE   VK27                                                             
         BAS   RE,VALINV           VALIDATE INVOICE #                           
         B     VK30                                                             
*                                                                               
VK27     CLI   COMTYPE,TLCMTADV    IF COMMENT TYPE IS ADVICE (DETAILS)          
         BNE   ERRINV                                                           
         BAS   RE,VALADV           VALIDATE ADVICE COML ID AND ADV CODE         
         B     VK30                                                             
*                                                                               
VK30     GOTO1 RECVAL,DMCB,TLCMCDQ,(X'40',COMTYPEH)  BUILD KEY                  
         CLI   COMLEV,C'C'         IF NOT CLIENT LEVEL C,D, OR F                
         BE    VK60                                                             
         CLI   COMFLIP,C'T'        AND READING TP COMMENTS                      
         BNE   VK60                SET TP FLAG IN KEY                           
VK50     LA    R4,KEY                                                           
         USING TLCMD,R4                                                         
         OI    TLCMLEV,TLCMTPC     SET TALENT PARTNER COMMENT IN KEY            
*                                                                               
VK60     MVC   SVKEY,KEY                                                        
*                                                                               
         CLI   COMTYPE,TLCMTCOM    IF COMMENT TYPE IS COMMERCIAL                
         BNE   VK69                                                             
         CLI   TLCMVER,1           AND VERSION IS SET AS 1                      
         BNE   VK69                                                             
         GOTO1 HIGH                                                             
         CLC   TLCMKEY,KEYSAVE     SEE IF IT EXISTS                             
         BE    VK69                                                             
         MVC   KEY,SVKEY           IF NOT, SET AS VERSION 0 INSTEAD             
         MVI   TLCMVER,0                                                        
         MVC   SVKEY,KEY                                                        
         DROP  R4                                                               
                                                                                
VK69     CLI   ACTNUM,ACTDIS       CLEAR SCREEN BEFORE DISPLAY/CHANGE           
         BE    *+12                SO IF RECORD NOT FOUND AFTER FLIP,           
         CLI   ACTNUM,ACTCHA       SCREEN WILL BE CLEARED                       
         BNE   VK70                                                             
         GOTO1 FLDVAL,DMCB,(X'01',COMLIN1H),(X'80',999)  CLEAR SCREEN           
         GOTO1 FLDVAL,DMCB,(X'01',COMLCHGH),1                                   
*                                                                               
VK70     GOTO1 FLDVAL,DMCB,(X'20',COMAGYH),(X'80',COMENDH)                      
*                                                                               
         CLI   SVRETRN,C'O'        IF CAME HERE FROM ADVICE/COMPLETE            
         BNE   VKX                                                              
         CLI   ACTNUM,ACTADD       AND ACTION IS ADD                            
         BNE   VKX                                                              
         GOTO1 HIGH                READ KEY NOW                                 
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         MVC   KEY,KEYSAVE                                                      
         BNE   VKX                                                              
         MVC   CONACT(6),=CL6'CHANGE'                                           
         MVI   CONACTH+5,5                                                      
         OI    CONACTH+6,X'80'                                                  
         MVI   ACTNUM,ACTCHA                                                    
VKX      B     XIT                                                              
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
DK       MVC   SVKEY,KEY                                                        
         L     R4,AIO                                                           
         USING TLCMD,R4                                                         
         MVC   COMAGY,TLCMAGY      AGENCY                                       
         OI    COMAGYH+6,X'80'                                                  
         MVC   COMTYPE,TLCMTYP                                                  
         OI    COMTYPEH+6,X'80'                                                 
         CLI   TLCMTYP,TLCMTGUA    WHICH TYPE?                                  
         BNE   DK10                                                             
         MVC   COMID,TLCMCID       GUARANTEE TYPE                               
         OI    COMIDH+6,X'80'                                                   
         MVC   COMCODE,SPACES                                                   
         MVC   COMCODE(L'TLCMGUA),TLCMGUA                                       
         OI    COMCODEH+6,X'80'                                                 
         B     XIT                                                              
DK10     MVC   COMID,TLCMSSN       COMMERCIAL TYPE                              
         OI    COMIDH+6,X'80'                                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'01',COMLIN1H),(X'80',999)  CLEAR SCREEN           
*                                                                               
         LA    R2,COMLIN1H         R2=A(FIRST COMMENT FIELD)                    
         LA    R5,16               R5=# OF COMMENT LINES                        
         LA    R6,1                R6=COMMENT FIELD COUNTER                     
DIS10    L     R4,AIO              R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAXCELQ                                                   
         BAS   RE,GETEL            R4=A(COMMENT ELEMENT)                        
         B     *+8                                                              
DIS20    BAS   RE,NEXTEL                                                        
         BNE   DIS30                                                            
         USING TAXCD,R4                                                         
         ZIC   RE,TAXCSEQ          MATCH UP SEQUENCE # IN ELEMENT               
         CR    R6,RE               TO SEQUENCE # OF COMMENT FIELD               
         BNE   DIS20                                                            
         ZIC   RE,TAXCLEN                                                       
         SHI   RE,4                                                             
         STC   RE,5(R2)            PUT LENGTH TO SCREEN                         
         OI    6(R2),X'80'         TRANSMIT                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),TAXCCMNT    DISPLAY COMMENT                              
DIS30    ZIC   RE,0(R2)                                                         
         AR    R2,RE               BUMP TO NEXT COMMENT FIELD                   
         LA    R6,1(R6)            INCREMENT COMMENT FIELD COUNTER              
         BCT   R5,DIS10                                                         
         DROP  R4                                                               
*                                                                               
         GOTO1 ACTVOUT,DMCB,COMLCHGH               LAST CHANGED                 
*                                                                               
         CLI   COMTYPE,TLCMTINV    IF INVOICE COMMENT                           
         BNE   XIT                                                              
         CLC   COMLIN1(14),=CL14'DELETE REASON:'                                
         BNE   XIT                                                              
         MVI   SVRECUSE,C'O'                                                    
         MVI   SVRETRN,C'O'                                                     
         BAS   RE,SETPFK                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              VALIDATE WEB APPLICATION ID                                      
         SPACE 1                                                                
VALWID   NTR1                                                                   
         CLC   =C'VC',SVWID         ENSURE RECORD IS NOT STAMPED                
         BE    VWID10               WITH VITA COMPLETIONS ID                    
         CLC   =C'TC',SVWID                                                     
         BE    VWID10                                                           
         CLC   =C'RC',SVWID                                                     
         BNE   XIT                                                              
VWID10   CLI   COMFLIP,C'T'                                                     
         BE    WEBERR                                                           
         B     XIT                                                              
         SPACE 1                                                                
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         CLI   COMTYPE,TLCMTINV     CANNOT CHANGE COMMENT FOR                   
         BNE   BLDR05               DELETED INVOICES                            
         CLI   DELINV,C'Y'                                                      
         BE    NOCHANGE                                                         
         SPACE 1                                                                
BLDR05   MVI   ELCODE,TAXCELQ       DELETE ALL OLD COMMENT ELEMENTS             
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         LA    R2,COMLIN1H                         COMMENTS                     
         LA    R4,16                R4 = # OF COMMENT FIELDS                    
         LA    R5,1                 R5 = SEQUENCE NUMBER                        
BLDR10   CLI   5(R2),0                                                          
         BE    BLDR20                                                           
         XC    ELEMENT,ELEMENT      BUILD ELEMENT FOR EACH                      
         LA    R3,ELEMENT           COMMENT FIELD WITH DATA                     
         USING TAXCD,R3                                                         
         MVI   TAXCEL,TAXCELQ       ELEMENT CODE                                
         ZIC   RE,5(R2)                                                         
         AHI   RE,4                                                             
         STC   RE,TAXCLEN           ELEMENT LENGTH                              
         STC   R5,TAXCSEQ           SEQUENCE NUMBER                             
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)    COMMENT                                     
         GOTO1 ADDELEM                                                          
BLDR20   ZIC   RE,0(R2)             BUMP TO NEXT COMMENT FIELD                  
         AR    R2,RE                                                            
         LA    R5,1(R5)             INCREMENT SEQUENCE NUMBER                   
         BCT   R4,BLDR10                                                        
         DROP  R3                                                               
*                                                                               
         GOTO1 ACTVIN,DMCB,COMLCHGH                LAST CHANGED                 
         B     XIT                                                              
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
*              CHANGE SCREEN TO RETURN TO IF NECESSARY                          
*                                                                               
CHGRETRN NTR1                                                                   
         LA    R4,KEY                                                           
         USING TLCMD,R4                                                         
         CLI   TLCMTYP,TLCMTCOM    IF COMMENT ON SCREEN IS COMMERCIAL           
         BNE   CRET20              TYPE, MAKE SURE WE ARE RETURNING             
         CLI   SVRETRN,CO          TO COMMERCIAL OR HISTORY SCREENS             
         BE    XIT                                                              
         CLI   SVRETRN,HI                                                       
         BE    XIT                                                              
         MVI   SVRETRN,CO                                                       
         B     XIT                                                              
CRET20   CLI   TLCMTYP,TLCMTGUA    IF COMMENT ON SCREEN IS GUARANTEE            
         BNE   CRET30              TYPE, MAKE SURE WE ARE RETURNING             
         CLI   SVRETRN,GU          TO GUARANTEE OR GTRACK SCREENS               
         BE    XIT                                                              
         CLI   SVRETRN,GT                                                       
         BE    XIT                                                              
         MVI   SVRETRN,GU                                                       
         B     XIT                                                              
CRET30   CLI   TLCMTYP,TLCMTCON    IF COMMENT ON SCREEN IS CONTRACT             
         BNE   CRET40              TYPE, SET TO RETURN TO CONTRACT SCRN         
         MVI   SVRETRN,ON                                                       
         B     XIT                                                              
CRET40   CLI   TLCMTYP,TLCMTINV    IF COMMENT ON SCREEN IS INVOICE              
         BNE   CRET50              TYPE, SET TO RETURN TO INVOICE SCRN          
*        MVI   SVRETRN,IN                                                       
         B     XIT                                                              
CRET50   CLI   TLCMTYP,TLCMTADV    IF COMMENT ON SCREEN IS ADVICE               
         BNE   XIT                 TYPE, SET TO RETURN TO ADVICE SCRN           
         MVI   SVRETRN,DV                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              CHECK RECORD TYPE AGAINST COMMENT TYPE                           
*                                                                               
CHKREC   NTR1                                                                   
         CLI   5(R2),0             EXIT IF NOT INPUT IN THE                     
         BE    CSX                 COMMENT TYPE FIELD                           
*                                                                               
         LA    R2,COMTYPEH                                                      
         CLC   =C'DETAILS',CONREC  IF DETAILS IS THE RECORD TYPE                
         BNE   CS10                                                             
         CLI   COMTYPE,TLCMTADV    COMMENT TYPE MUST BE ADVICE                  
         BNE   ERRINV                                                           
*                                                                               
CS10     CLC   =C'COMMENT',CONREC  IF COMMENT IS THE RECORD TYPE                
         BNE   CSX                                                              
         CLI   COMTYPE,TLCMTADV    COMMENT TYPE CANNOT BE ADVICE                
         BE    ERRINV                                                           
CSX      B     XIT                                                              
*                                                                               
*              SET COMMENT FLIP SWITCH TO TP OR CLIENT LEVEL                    
*                                                                               
SETCMLEV NTR1                                                                   
         CLI   COMLEV,C'T'         IF TP LEVEL                                  
         BNE   SCLEV05                                                          
         CLI   COMFLIP,C'C'        BUT READING CLIENT COMMENTS                  
         BNE   SCLEV05                                                          
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTCHA       ACTION CHANGE NOT ALLOWED                    
         BE    ERRINV                                                           
         CLI   ACTNUM,ACTDEL       ACTION DELETE NOT ALLOWED                    
         BE    ERRINV                                                           
         CLI   ACTNUM,ACTADD       IF ACTION ADD, SET FLAG FOR TP CMNT          
         BNE   SCLEV05                                                          
         MVI   COMFLIP,C'T'                                                     
SCLEV05  CLI   TGCTSTTY,TASTTYPC   IF CLIENT LEVEL C,D, OR F                    
         BE    SCLEV10                                                          
         CLI   TGCTSTTY,TASTTYPD                                                
         BE    SCLEV10                                                          
         CLI   TGCTSTTY,TASTTYPF                                                
         BNE   SCLEV20                                                          
SCLEV10  MVI   COMLEV,C'C'         CLIENT LEVEL                                 
         MVI   COMFLIP,C'C'        SET TO READ CLIENT COMMENT                   
         CLI   PFAID,14            CLIENTS CANNOT HIT PF14                      
         BNE   XIT                                                              
         B     INVPFK                                                           
*                                  NON-CLIENT LEVEL                             
SCLEV20  MVI   COMLEV,C'T'         TALENT PARTNERS LEVEL                        
         CLI   COMFLIP,C'T'        1ST TIME IN - NOT T OR C                     
         BE    SCLEV30                                                          
         CLI   COMFLIP,C'C'                                                     
         BE    SCLEV30                                                          
         MVI   COMFLIP,C'T'        SET TO READ TP COMMENTS                      
         B     XIT                                                              
SCLEV30  CLI   PFAID,14            IF PF14 HIT, FLIP BACK AND FORTH             
         BNE   XIT                 FROM TP COMMENT TO CLIENT COMMENT            
*                                                                               
*        OC    TGGUA,TGGUA         IF THERE IS A GLOBAL GUAR CODE               
*        BZ    SCLEV35                                                          
*        MVC   SVGUAR,TGGUA        MOVE TO ANOTHER FIELD                        
*        XC    SVGUAR,HEXFFS       AND UNCOMPLEMENT IT                          
*                                                                               
SCLEV35  CLI   COMFLIP,C'T'        IF TP COMMENT WAS JUST READ,                 
         BNE   SCLEV40                                                          
         MVI   COMFLIP,C'C'        SET FOR CLIENT COMMENT                       
         B     XIT                                                              
SCLEV40  CLI   COMFLIP,C'C'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   COMFLIP,C'T'        OTHERWISE SET FOR TP COMMENT                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              SET PFTABLE TO POINT TO FOR INITIAL CALL                         
*              ADDRESS OF PFTABLE IS RETURNED IN R3                             
*                                                                               
SETPFTB  NTR1                                                                   
         LA    R3,PFTABG           RETURN TO WHICH SCREEN?                      
         CLI   SVRETRN,GU          GUARANTEE                                    
         BE    SPFTBX                                                           
         LA    R3,PFTABC                                                        
         CLI   SVRETRN,CO          COMMERCIAL                                   
         BE    SPFTBX                                                           
         LA    R3,PFTABI                                                        
         CLI   SVRETRN,IN          INVOICE                                      
         BE    SPFTBX                                                           
         LA    R3,PFTABO           ADVICE/COMPLETE                              
         CLI   SVRETRN,C'O'                                                     
         BE    SPFTBX                                                           
         LA    R3,PFTABN                                                        
         CLI   SVRETRN,DI          DINVOICE                                     
         BE    SPFTBX                                                           
         LA    R3,PFTABH                                                        
         CLI   SVRETRN,HI          HISTORY (COMMENT TYPE C)                     
         BNE   SPFTB20                                                          
         CLI   TGINV,X'20'                                                      
         BNH   *+10                                                             
         XC    TGINV,=6X'FF'       UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,TGINV,INV,DATCON CNV FOR DISPLAY                    
         B     SPFTBX                                                           
SPFTB20  LA    R3,PFTABT                                                        
         CLI   SVRETRN,GT          GTRACK (COMMENT TYPE G)                      
         BE    SPFTBX                                                           
         LA    R3,PFTABA                                                        
         CLI   SVRETRN,ON          CONTRACT (AGREEMENT)                         
         BE    SPFTBX                                                           
         LA    R3,PFTABV                                                        
         CLI   SVRETRN,DV          ADVICE (COMMENT TYPE V)                      
         BE    SPFTBX                                                           
         LA    R3,0                WE DON'T KNOW THE TYPE YET                   
SPFTBX   XIT1  REGS=(R3)           RETURN ADDRESS OF PFTABLE IN R3              
         EJECT                                                                  
*                                                                               
*              SET PFKEY LINE                                                   
*                                                                               
SETPFK   NTR1                                                                   
         OI    COMPF13H+6,X'80'                                                 
         MVC   COMPF13(L'RETURN),RETURN                                         
         CLI   SVRECUSE,C'O'                                                    
         BNE   SPFK10                                                           
         MVC   COMPF13(L'INVDEL),INVDEL                                         
*                                                                               
SPFK10   CLI   COMLEV,C'T'         IF NOT TP LEVEL (CLIENT)                     
         BNE   XIT                 DON'T EVER SHOW PF14                         
         MVC   COMPFFL,=CL24'PF14=Flip to Client Cmnt'                          
         CLI   COMFLIP,C'T'                                                     
         BE    *+10                                                             
         MVC   COMPFFL,=CL24'PF14=Flip to TP Cmnt'                              
         OI    COMPFFLH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE COMMERCIAL TYPE COMMENT RECORD                          
*                                                                               
VALCOML  NTR1                                                                   
         LA    R2,COMTYPEH                                                      
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'20',COMIDH)                              
*                                                                               
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGCOM,TLCOCOM       SET INTERNAL COMMERCIAL NUMBER               
         DROP  R4                                                               
*                                                                               
         LA    R2,COMIDH           POINT TO THE ID FIELD                        
         L     R4,AIO                                                           
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TACOMED,TACOMEDE    EVENT?                                       
         BE    ERRNTF              YES, ERROR RECORD NOT FOUND                  
                                                                                
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   VCOML10                                                          
         L     R4,TGELEM                                                        
         MVC   SVWID,TAFNNAME                                                   
         DROP  R4                                                               
*                                                                               
VCOML10  LA    R2,COMCODEH         CODE FIELD SHOULD BE BLANK                   
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
                                                                                
         LA    R2,COMSTAH          START AND END DATES SHOULD BE BLANK          
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
         LA    R2,COMENDH                                                       
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE CONTRACT TYPE COMMENT RECORD                            
*                                                                               
VALCON   NTR1                                                                   
         LA    R2,COMCODEH         CODE FIELD SHOULD BE BLANK                   
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
*                                                                               
         LA    R2,COMIDH           VALIDATE CONTRACT ID                         
         OC    COMID,SPACES                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCND,R4                                                         
         MVI   TLCNCD,TLCNCDQ                                                   
         MVC   TLCNAGY,TGAGY                                                    
         MVC   TLCNCNID,COMID                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCNTRMS-TLCND),KEYSAVE                                      
         BNE   ERRINV                                                           
         MVC   TGCNID,COMID        SAVE IN GLOBAL                               
         DROP  R4                                                               
*                                                                               
         LA    R2,COMSTAH                                                       
         GOTO1 DTVAL,DMCB,SVSTART  VALIDATE TERM START AND END DATES            
         GOTO1 DATCON,DMCB,(1,SVSTART),(8,8(R2))                                
         MVC   TGCNTRMS,SVSTART    SAVE IN GLOBAL                               
         OI    4(R2),X'80'                                                      
         MVC   SVCNSTA,COMSTA      STORE DATE FOR PFKEY FLIP                    
         LA    R2,COMENDH                                                       
         GOTO1 DTVAL,DMCB,SVEND                                                 
         GOTO1 DATCON,DMCB,(1,SVEND),(8,8(R2))                                  
         MVC   TGCNTRME,SVEND      SAVE IN GLOBAL                               
         OI    4(R2),X'80'                                                      
         MVC   SVCNEND,COMEND      STORE DATE FOR PFKEY FLIP                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE INVOICE TYPE COMMENT RECORD                             
*                                                                               
VALINV   NTR1                                                                   
         LA    R2,COMIDH                                                        
         CLI   5(R2),6                                                          
         BNE   ERRINV                                                           
         GOTO1 TINVCON,DMCB,COMID,INV,DATCON                                    
         CLI   0(R1),X'FF'                                                      
         BE    ERRINV                                                           
         XC    INV,=6X'FF'                                                      
         GOTO1 RECVAL,DMCB,(X'08',TLINCDQ),(X'84',INV)                          
         BNE   ERRINV                                                           
         SPACE 1                                                                
         USING TLDRD,RE                                                         
         MVI   DELINV,C'N'        SET INVOICE DELETED INDICATOR                 
         LA    RE,KEY                                                           
         CLI   TLDRSTAT,0                                                       
         BE    XIT                                                              
         MVI   DELINV,C'Y'                                                      
         DROP  RE                                                               
         B     XIT                                                              
*                                                                               
*              VALIDATE ADVICE TYPE COMMENT (DETAILS) RECORD                    
*                                                                               
VALADV   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLCOICDQ,COMIDH                                      
         GOTO1 RECVAL,DMCB,TLDVCDQ,COMCODEH                                     
         B     XIT                                                              
         EJECT                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO DO EXTRA PROCESSING IF COMING FROM ADV/COMP           
*                                                                               
COMPPRO  NTR1                                                                   
         CLI   COMTYPE,TLCMTINV        IF COMMENT DOES NOT APPEAR TO            
         BNE   XIT                     BE AN INVOICE'S DELETE REASON            
         CLI   SVRETRN,C'O'            EXIT                                     
         BNE   XIT                                                              
*                                                                               
         CLI   MODE,XRECADD            IF JUST ADDED COMMENT RECORD             
         BNE   CPRO10                                                           
         MVC   CONACT(6),=CL6'CHANGE'  CHANGE ACTION TO 'CHANGE'                
         MVI   CONACTH+5,5                                                      
         OI    CONACTH+6,X'80'                                                  
         B     RECDISPC                                                         
*                                                                               
         USING TLRCD,RE                                                         
CPRO10   CLI   ACTNUM,ACTCHA           IF JUST ADDED OR PUT COMMENT             
         BNE   CPRO20                                                           
         L     RE,AIO                  COMMENT MUST BE AT LEAST                 
         CLC   TLRCLEN,=H'97'          25 CHARACTERS LONG                       
         BL    NODELETE                                                         
         CLI   SVRETRN,C'O'                                                     
         BE    HITPF13                                                          
*                                                                               
CPRO20   CLI   ACTNUM,ACTDIS                                                    
         BE    RECDISP                                                          
         B     XIT                                                              
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ERRORS                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
ERRMIS   MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
ERRACT   MVI   ERROR,INVACT        INVALID ACTION                               
         B     THEEND                                                           
*                                                                               
INVPFK   MVI   ERROR,ERINVPFK     INVALID PRKEY                                 
         B     THEEND                                                           
*                                                                               
ERRNTF   MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
*                                                                               
WEBERR   MVC   MYMSGNO,=Y(ERUSEWEB) RECORD MUST BE UPDATED FROM                 
         J     ERREND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   BLOCK,0              WEB APPLICATION                             
         MVI   MYMTYP,GTMERR                                                    
         B     INFEND                                                           
*                                                                               
RECDISP  MVI   MYMSGNO1,252        RECORD DISPLAYED-ENTER NEXT REQ              
         B     LIN1DISP                                                         
*                                                                               
RECDISPC MVI   MYMSGNO1,107        RECORD DISPLAYED-NOW ENTER CHANGES           
         B     LIN1DISP                                                         
*                                                                               
NODELETE MVC   MYMSGNO,=Y(ERINVCML) INVOICE COMMENT NOT LONG ENOUGH             
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     LIN1DISP                                                         
*                                                                               
NOCHANGE MVI   MYMSGNO1,254         CANNOT CHANGE DELETED INV COMMENT           
         B     INFEND                                                           
*                                                                               
HITPF13  MVI   MYMSGNO1,250         HIT PF13 TO GO TO INVOICE/DELETE            
         B     LIN1DISP                                                         
*                                                                               
LIN1DISP LA    R2,COMLIN1H                                                      
         MVI   ERRDISP,14                                                       
         B     INFEND                                                           
*                                                                               
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
HEXFFS   DC    XL4'FFFFFFFF'                                                    
RETURN   DC    CL19'PF13=Return'                                                
INVDEL   DC    CL19'PF13=Invoice/Delete'                                        
         SPACE 2                                                                
PFTABG   DS    0C                                                               
         DC    AL1(PFG13X-*,13,0,(PFG13X-PFG13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'GRT',CL8'DISPLAY'                                   
PFG13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGGUA-1),AL2(TGGUA-TGD)                           
PFG13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFG14X-*,14,0,(PFG14X-PFG14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFG14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYTWA,L'SVGUAR-1),AL2(SVGUAR-T702FFD)                     
PFG14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABC   DS    0C                                                               
         DC    AL1(PFC13X-*,13,0,(PFC13X-PFC13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMERCL',CL8'DISPLAY'                              
PFC13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PFC13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFC14X-*,14,0,(PFC14X-PFC14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFC14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PFC14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABI   DS    0C                                                               
         DC    AL1(PFI13X-*,13,0,(PFI13X-PFI13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'INVOICE ',CL8'DISPLAY'                              
PFI13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'INV-1),AL2(COMID-T702FFD)                         
PFI13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFI14X-*,14,0,(PFI14X-PFI14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFI14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYTWA,L'INV-1),AL2(COMID-T702FFD)                         
PFI14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABO   DS    0C                                                               
         DC    AL1(PFO13X-*,13,0,(PFO13X-PFO13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'INVOICE ',CL8'DELETE '                              
PFO13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'INV-1),AL2(COMID-T702FFD)                         
PFO13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFO14X-*,14,0,(PFO14X-PFO14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFO14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYTWA,L'INV-1),AL2(COMID-T702FFD)                         
PFO14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABN   DS    0C                                                               
         DC    AL1(PFN13X-*,13,0,(PFN13X-PFN13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'DINVOICE',CL8'DISPLAY'                              
PFN13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'INV-1),AL2(COMID-T702FFD)                         
PFN13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFN14X-*,14,0,(PFN14X-PFN14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFN14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYTWA,L'INV-1),AL2(COMID-T702FFD)                         
PFN14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABA   DS    0C                                                               
         DC    AL1(PFA13X-*,13,0,(PFA13X-PFA13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'CONTRACT',CL8'DISPLAY'                              
PFA13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCNID-1),AL2(TGCNID-TGD)                         
         DC    AL1(KEYTYTWA,L'COMSTA-1),AL2(COMSTA-T702FFD)                     
         DC    AL1(KEYTYTWA,L'COMEND-1),AL2(COMEND-T702FFD)                     
PFA13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFA14X-*,14,0,(PFA14X-PFA14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFA14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYGLB,L'TGCNID-1),AL2(TGCNID-TGD)                         
         DC    AL1(KEYTYTWA,L'SVCNSTA-1),AL2(SVCNSTA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'SVCNEND-1),AL2(SVCNEND-T702FFD)                   
PFA14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABH   DS    0C                                                               
         DC    AL1(PFH13X-*,13,0,(PFH13X-PFH13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'HISTORY',CL8'DISPLAY'                               
PFH13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'INV-1),AL2(INV-T702FFD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PFH13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFH14X-*,14,0,(PFH14X-PFH14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFH14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PFH14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABT   DS    0C                                                               
         DC    AL1(PFT13X-*,13,0,(PFT13X-PFT13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'GTRACK',CL8'DISPLAY'                                
PFT13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGGUA-1),AL2(TGGUA-TGD)                           
PFT13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFT14X-*,14,0,(PFT14X-PFT14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COMMENT',CL8'DISPLAY'                               
PFT14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYTWA,L'SVGUAR-1),AL2(SVGUAR-T702FFD)                     
PFT14X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
PFTABV   DS    0C                                                               
         DC    AL1(PFV13X-*,13,0,(PFV13X-PFV13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'ADVICE  ',CL8'DISPLAY'                              
PFV13    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYGLB,L'TGADV-1),AL2(TGADV-TGD)                           
PFV13X   EQU   *                                                                
*                                                                               
         DC    AL1(PFV14X-*,14,0,(PFV14X-PFV14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'DETAILS',CL8'DISPLAY'                               
PFV14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGTYPE-1),AL2(TGTYPE-TGD)                         
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYGLB,L'TGADV-1),AL2(TGADV-TGD)                           
PFV14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              SET UP SCREEN FIELDS DEPENDING ON COMMENT TYPE                   
*                                                                               
SETSCRN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'04',COMCODEH),1 UNPROTECT                         
         GOTO1 FLDVAL,DMCB,(X'04',COMSTAH),1                                    
         GOTO1 FLDVAL,DMCB,(X'04',COMENDH),1                                    
         GOTO1 FLDVAL,DMCB,COMCODH,(X'60',COMENDH) HIGH INTENSITY               
*                                                                               
         CLI   COMTYPE,TLCMTCOM    IF COMMENT TYPE IS COMMERCIAL                
         BE    *+12                                                             
         CLI   COMTYPE,TLCMTINV    OR INVOICE                                   
         BNE   SSCRN10                                                          
         GOTO1 FLDVAL,DMCB,COMCODH,(X'48',COMENDH) LOW INTENSITY                
         GOTO1 FLDVAL,DMCB,(X'09',COMCODEH),1      CLR/PROTECT                  
         GOTO1 FLDVAL,DMCB,(X'09',COMSTAH),1       CLR/PROTECT                  
         GOTO1 FLDVAL,DMCB,(X'09',COMENDH),1       CLR/PROTECT                  
*                                                                               
SSCRN10  CLI   COMTYPE,TLCMTGUA    IF COMMENT TYPE IS GUAR                      
         BE    *+12                                                             
         CLI   COMTYPE,TLCMTADV    OR ADVICE                                    
         BNE   SSCRN20                                                          
         GOTO1 FLDVAL,DMCB,COMSTH,(X'48',COMENDH)  LOW INTENSITY                
         GOTO1 FLDVAL,DMCB,(X'09',COMSTAH),1       CLR/PROT                     
         GOTO1 FLDVAL,DMCB,(X'09',COMENDH),1       CLR/PROT                     
*                                                                               
SSCRN20  CLI   COMTYPE,TLCMTCON    IF COMMENT TYPE IS CONTRACT                  
         BNE   SSCRNX                                                           
         GOTO1 FLDVAL,DMCB,(X'04',COMSTAH),1    UNPROTECT                       
         GOTO1 FLDVAL,DMCB,(X'04',COMENDH),1    UNPROTECT                       
         GOTO1 FLDVAL,DMCB,COMSTH,(X'60',COMENH)   HIGH INTENSITY               
         GOTO1 FLDVAL,DMCB,COMCODH,(X'48',1)  LOW INTENSITY                     
         GOTO1 FLDVAL,DMCB,(X'09',COMCODEH),1    CLR/PROT                       
*                                                                               
SSCRNX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCREDD                                                       
         ORG   COMWORK                                                          
*                                                                               
SVKEY    DS    CL38                                                             
INV      DS    CL6               INVOICE NUMBER (PRINTABLE)                     
COMLEV   DS    CL1               LEVEL OF COMMENT (TP OR CLIENT)                
COMFLIP  DS    CL1               COMMENT FLIP SWITCH                            
SVGUAR   DS    CL4               SAVED GUARANTEE CODE                           
SVSTART  DS    XL3               SAVED TERM START DATE                          
SVEND    DS    XL3               SAVED TERM END DATE                            
SVRETRN  DS    CL1               RETURN TO THIS REC/USE SCREEN                  
SVCNSTA  DS    CL8               SAVED CONTRACT START DATE                      
SVCNEND  DS    CL8               SAVED CONTRACT END DATE                        
DELINV   DS    CL1                                                              
SVWID    DS    CL18                SAVED WEB APPLICATION ID                     
*                                                                               
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         ORG   TWAHOLE                                                          
SVRECUSE DS    CL1                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079TAGENED   07/07/14'                                      
         END                                                                    
