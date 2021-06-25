*          DATA SET SRPFM00    AT LEVEL 014 AS OF 05/01/02                      
*PHASE T13700A                                                                  
*INCLUDE DECODE                                                                 
         TITLE 'PFM00 - ROOT CONTROLLER AND TABLES'                             
         PRINT NOGEN                                                            
PFM00    CSECT                                                                  
         NMODL PFMTEMPX-PFMTEMPD,**PF00**,RR=R5                                 
         LR    R9,RC                                                            
         USING PFMTEMPD,R9         R9=A(GLOBAL W/S)                             
         ST    R5,RELO                                                          
*                                  RELOCATE EXTERNALS                           
RELOC    L     R4,=V(SYSTBL)                                                    
         AR    R4,R5                                                            
         ST    R4,ASYSTBL                                                       
         L     R4,=V(FILETBL)                                                   
         AR    R4,R5                                                            
         ST    R4,AFILETBL                                                      
         L     R4,=V(PERMTBL)                                                   
         AR    R4,R5                                                            
         ST    R4,APERMTBL                                                      
         L     R4,=V(RCVRTBL)                                                   
         AR    R4,R5                                                            
         ST    R4,ARCVRTBL                                                      
         L     R4,=V(DECODE)                                                    
         AR    R4,R5                                                            
         ST    R4,ADECODE                                                       
*                                  RELOCATE ROUTINES                            
RELOC1   LA    R4,OLAY                                                          
         ST    R4,AOLAY                                                         
         LA    R4,CLEAR                                                         
         ST    R4,ACLEAR                                                        
         LA    R4,DISP                                                          
         ST    R4,ADISP                                                         
         LA    R4,DISKIO                                                        
         ST    R4,ADISKIO                                                       
*                                  RELOCATE TABLES                              
RELOC2   MVC   VWDISP,WDISP                                                     
         MVC   VNDISP,NDISP                                                     
         LA    R4,KEYTBL                                                        
         ST    R4,AKEYTBL                                                       
         LA    R4,ACTNTBL                                                       
         ST    R4,AACTNTBL                                                      
         LA    R4,DMCMDS                                                        
         ST    R4,ADMCMDS                                                       
         L     R4,=A(DISPTBLL)     LOWER CASE                                   
         AR    R4,R5                                                            
RELOCX   ST    R4,ADISPTBL                                                      
*                                  INITIALISE                                   
INIT     L     R3,20(R1)                                                        
         USING PFMSAVED,R3         R3=A(TWA)                                    
         ST    R3,ASAVE                                                         
         ST    R1,APARM                                                         
         ST    RB,ABASE                                                         
         ST    R9,ATEMP                                                         
         L     R4,12(R1)                                                        
         USING COMFACSD,R4                                                      
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         L     R4,0(R1)             EXTRACT SSB TEMPSTR/TEMPEST INFO            
         USING SYSFACD,R4                                                       
         L     R4,VSSB                                                          
         USING SSBD,R4                                                          
         ST    R4,ASSB                                                          
         MVC   TWAL,SSBTWAL                                                     
         MVC   TMSL,SSBTMSL                                                     
         MVC   TWACHKD,=H'16960'   18K TEMPSTR RECS =Y(CHKPTDSP)                
         MVC   TWAGBLD,=H'16448'                    =Y(CHKPTGLD)                
         CLC   TWAL,=H'14336'                                                   
         BNE   INIT1                                                            
         MVC   TWACHKD,=H'12800'   14K TEMPSTR RECS                             
         MVC   TWAGBLD,=H'12800'                                                
         DROP  R4                                                               
*                                                                               
INIT1    MVI   BLANKS,C' '                                                      
         MVC   BLANKS+1(79),BLANKS                                              
         XC    FERRS,FERRS                                                      
         XC    SLIFINFO(046),SLIFINFO                                           
         XC    SLIRINFO(124),SLIRINFO                                           
*                                                                               
INIT2    CLC   ISRVH+9(4),=C'TFM,'  TEST $TFM,00 THRU $TFM,0B FOR TWA           
         BNE   INIT3                                                            
         CLI   ISRVH+13,C'0'                                                    
         BNE   INIT3                                                            
         CLI   ISRVH+14,C'A'                                                    
         BL    INIT3                                                            
         CLI   ISRVH+14,C'B'                                                    
         BNH   INIT2A                                                           
         CLI   ISRVH+14,C'0'                                                    
         BL    INIT3                                                            
         CLI   ISRVH+14,C'9'                                                    
         BH    INIT3                                                            
INIT2A   MVI   IFILEH+5,7                                                       
         MVC   IFILEH+8(7),=C'TEMPSTR'                                          
         MVI   IRIDH+5,4                                                        
         MVC   IRIDH+8(2),=C'K,'                                                
         MVC   IRIDH+10(2),ISRVH+13                                             
         XC    ISRVH+12(3),ISRVH+12                                             
*                                                                               
INIT3    EQU   *                                                                
         EJECT                                                                  
*                                  INPUT IS REC DEFN & ACTION                   
STAT0    MVI   OLAYOP,1            OVERLAY NUM 1                                
         GOTO1 AOLAY               SYNTAX INPUT & DISPLAY                       
         TM    FIND,X'01'                                                       
         BO    OHDRMSG             DISPLAY ERROR MSG & EXIT                     
         MVI   STATUS,1                                                         
         CLI   STIRA,1             DIS                                          
         BE    TRANSEND                                                         
         CLI   STIRA,2             CHA                                          
         BE    STAT0A                                                           
         CLI   STIRA,3             ADD                                          
         BE    STAT0A                                                           
         CLI   STIRA,4             BRO                                          
         BE    TRANSEND                                                         
         CLI   STIRA,5             REP                                          
         BE    STAT1A                                                           
STAT0A   MVC   SLIFINFO,STIFINFO   SAVE FILE INFO                               
         MVC   SLIRINFO,STIRINFO   SAVE RECORD INFO                             
         MVC   SLIPINFO,STIP10     SAVE I/O INFO                                
         MVC   SLDISPDL,DISPDL     SAVE DISPLAYED DATE LENGTH                   
         B     OHDRMSG                                                          
         SPACE 2                                                                
*                                  INPUT IS UPDATE DATA                         
STAT1    MVC   STIFINFO,SLIFINFO   RESTORE SAVED INFO                           
         MVC   STIRINFO,SLIRINFO                                                
         MVC   STIP10(6),SLIPINFO                                               
         MVC   DISPDL,SLDISPDL                                                  
STAT1A   MVI   OLAYOP,2            OVERLAY NUM 2                                
         GOTO1 AOLAY               CHECK HEX & UPDATE                           
         TM    FIND,X'01'                                                       
         BO    OHDRMSG             DISPLAY ERROR MSG & EXIT                     
         MVI   STATUS,0            ALL OK SO RESET STATUS                       
         MVI   HDRN,0                                                           
         SPACE 2                                                                
TRANSEND MVC   SLDISPDL,DISPDL     END OF TRANSACTION                           
         MVC   SLIRA,STIRA                                                      
         MVI   STATUS,0                                                         
         B     OHDRMSG                                                          
         SPACE 2                                                                
OHDRMSG  XC    WSS,WSS             OUTPUT HEADER MESSAGE                        
         TM    FIND,X'01'                                                       
         BZ    OOK                                                              
OERROR   SR    R6,R6               SET WSS=ERROR NN - XXX...                    
         IC    R6,FERN                                                          
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   WSS(10),=C'ERROR NN -'                                           
         MVC   WSS+6(2),DUB+1                                                   
         SH    R6,=H'1'                                                         
         BNM   OERROR2                                                          
         L     R6,FERN             SET SPECIAL MESSAGE                          
         MVC   WSS+11(30),0(R6)                                                 
         B     OERROR3                                                          
OERROR2  XC    DUB(2),DUB                                                       
         MVI   DUB+1,L'ERRMSGS                                                  
         MH    R6,DUB                                                           
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         AR    R6,RF                                                            
         MVC   WSS+11(L'ERRMSGS),0(R6)                                          
OERROR3  L     R6,FERRS            SET CURSOR ON BAD FIELD                      
         OI    6(R6),OI1C                                                       
         B     OMSG                                                             
*                                                                               
OOK      SR    R6,R6               SET OK HEADER MESSAGE                        
         IC    R6,HDRN                                                          
         XC    DUB(2),DUB                                                       
         MVI   DUB+1,L'OKMSGS                                                   
         MH    R6,DUB                                                           
         L     RF,=A(OKMSGS)                                                    
         A     RF,RELO                                                          
         AR    R6,RF                                                            
         MVC   WSS(L'OKMSGS),0(R6)                                              
         CLI   STATUS,0            POSN CURSOR DEPENDING ON STATUS              
         BNE   *+12                                                             
         OI    IFILEH+6,OI1C                                                    
         B     OMSG                                                             
         CLI   STATUS,1                                                         
         BNE   *+16                                                             
         LA    R4,DLINE                                                         
         USING DLINED,R4                                                        
         OI    ODHH+6,OI1C                                                      
         DROP  R4                                                               
         B     OMSG                                                             
         DC    H'0'                                                             
OMSG     FOUT  OHDRH,WSS                                                        
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        THIS ROUTINE LOADS IN OVERLAY NO OLAYOP & PASSES CONTROL TO IT         
*                                                                               
OLAY     NTR1  BASE=ABASE                                                       
         LA    R1,COVWS                                                         
         USING CALLOVD,R1                                                       
         XC    OVPARM1,OVPARM1                                                  
         ST    R3,OVPARM2                                                       
         MVC   OVNO(1),OLAYOP                                                   
         L     RF,ACALLOV                                                       
         BASR  RE,RF                                                            
         CLI   OVERR,X'FF'                                                      
         BE    OLAYERR                                                          
         L     RF,OVPARM1                                                       
         BASR  RE,RF                                                            
         B     OLAYX                                                            
OLAYERR  DC    H'0'                                                             
OLAYX    XIT1                                                                   
         EJECT                                                                  
*        THIS ROUTINE NULLS OUT EACH FIELD IN THE DISPLAY LINE &                
*        SETS ON/OFF THE TRANSMIT BIT DEPENDING ON CLEAROP                      
*                                                                               
CLEAR    NTR1  BASE=ABASE                                                       
         LA    R4,DLINE                                                         
         USING DLINED,R4           R4=A(DISPLAY LINE)                           
         LH    R5,VNDISP           R5=NUM OF DISPLAY LINES                      
         LA    R6,L'DLINE          R6=LEN OF TWA DISPLAY LINE                   
         MVC   WSS,BLANKS          SET CLEAR VALUE      *****                   
CLEAR1   MVI   ODHH+7,L'ODH                                                     
         MVI   ODCH+7,L'ODC                                                     
         NI    ODHH+6,OI0T         SET OFF TRANSMIT                             
         NI    ODBH+6,OI0T                                                      
         NI    ODCH+6,OI0T                                                      
*        XC    ODHH+4(2),ODHH+4                                                 
*        XC    ODCH+4(2),ODCH+4                                                 
         CLI   CLEAROP,C'T'        CLEAROP = TRANSMIT OPTION                    
         BE    CLEAR2                                                           
         MVC   ODB,WSS                                                          
         MVC   ODH,WSS                                                          
         MVC   ODC,WSS                                                          
         B     CLEAR3                                                           
CLEAR2   CLC   ODB,WSS             SET ON TRANSMIT BITS                         
         BE    *+14                                                             
         MVC   ODB,WSS                                                          
         OI    ODBH+6,OI1T                                                      
         CLC   ODH,WSS                                                          
         BE    *+14                                                             
         MVC   ODH,WSS                                                          
         OI    ODHH+6,OI1T                                                      
         CLC   ODC,WSS                                                          
         BE    *+14                                                             
         MVC   ODC,WSS                                                          
         OI    ODCH+6,OI1T                                                      
CLEAR3   AR    R4,R6               UP TO NEXT LINE IN TWA                       
         BCT   R5,CLEAR1                                                        
CLEARX   XIT1                                                                   
         EJECT                                                                  
*        THIS ROUTINE DISPLAYS AS MUCH DATA OF THE RECORD IN IOAREA             
*        THAT WILL FIT ON SCREEN STARTING AT DISPLAY LINE NUM DISPSLN           
*        DISPOP BITS AS FOLLOWS  X'01'=1 DISP PART , X'01'=0 DISP WHOLE         
*        X'02'=1 DISP HEX , X'04'=1 DISP BYTES , X'08'=1 DISP CHARS             
*                                                                               
DISP     NTR1  BASE=ABASE                                                       
         LH    R4,DISPDL           CALLER SETS DISPDL                           
         SRDL  R4,32               R4&R5=DISP DATA LENGTH                       
         LH    R7,VWDISP                                                        
         STH   R7,DISPLW                                                        
         LH    R8,VNDISP                                                        
         STH   R8,DISPNL                                                        
         DR    R4,R7                                                            
         LTR   R4,R4                                                            
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         STH   R5,DISPNLR          R5=NO OF LINES REQ                           
         LH    R4,DISPSLN                                                       
         AR    R4,R5                                                            
         CH    R4,DISPNL                                                        
         BNH   DISP1                                                            
         MVI   DISPRES,1           SET DISPLAY ONLY PART                        
         TM    DISPOP,X'01'                                                     
         BZ    DISPX                                                            
         LH    R4,DISPNL                                                        
         SH    R4,DISPSLN                                                       
         STH   R4,DISPNLR          SET NEW LINES REQ TO REM                     
         MH    R4,DISPLW                                                        
         STH   R4,DISPDL           SET NEW DISPLAY LENGTH                       
         B     DISP2                                                            
DISP1    MVI   DISPRES,0           SET DISPLAY THE WHOLE                        
*                                  INATIALISE LOOP REGISTERS                    
DISP2    OI    DISPOP,X'80'        SET DATA DISPLAYED FLAG                      
         LA    R1,HEXWS            R1=A(HEXOUT W/S)                             
         USING HEXOUTD,R1                                                       
         LA    R7,=C'MIX'                                                       
         ST    R7,HOAO                                                          
         L     RF,AHEXOUT          RF=A(HEXOUT CSECT)                           
         LA    R4,L'DLINE                                                       
         MH    R4,DISPSLN                                                       
         LA    R4,DLINE(R4)        R4=A(DISPLAY LINE IN TWA)                    
         USING DLINED,R4                                                        
         LH    R5,DISPSB           CALLER SETS DISPSB                           
         LA    R5,IOAREA(R5)       R5=A(DATA IN IOAREA)                         
         LH    R6,DISPLW           R6=L'DISPLAY LINE DATA                       
         LH    R7,DISPNLR          R7=NUM OF DISP LINES REQ                     
         MVC   DISPCT,DISPDL       COUNT OF RESIDUAL BYTES                      
*                                  LOOP FOR EACH LINE DISPLAY                   
DISP3    CH    R7,=H'1'                                                         
         BNE   DISP4                                                            
         LH    R6,DISPCT           PARTIAL DISP FOR LAST LINE                   
DISP4    TM    DISPOP,X'02'                                                     
         BZ    DISP5                                                            
         ST    R5,HOAS             MOVE IN HEX                                  
         ST    R6,HOLS                                                          
         LA    R8,ODH                                                           
         ST    R8,HOAD                                                          
         BASR  RE,RF                                                            
*        MVC   ODHH+5(1),HOLD+3                                                 
         MVC   ODHH+7(1),HOLD+3                                                 
         OI    ODHH+6,OI1T                                                      
*                                                                               
DISP5    TM    DISPOP,X'04'                                                     
         BZ    DISP6                                                            
         LH    R8,DISPSB           SET UP SSSSS-FFFFF                           
         CVD   R8,DUB                                                           
         UNPK  DUB(5),DUB+5(3)                                                  
         OI    DUB+4,X'F0'                                                      
         MVC   ODB(5),DUB                                                       
         MVI   ODB+5,C' '                                                       
         CLC   DISPCT,DISPDL                                                    
         BNE   DISP6                                                            
         MVI   ODB+5,C'-'          SET 1ST LINE FULL                            
         OI    ODBH+1,X'08'        SET HIGH INTENSITY                           
         MVC   ODBH+4(2),=X'0C07'                                               
         CLI   DISPRES,1                                                        
         BNE   DISP6                                                            
         MVI   ODB+5,C'*'          SET 1ST LINE PART                            
         OI    ODBH+1,X'08'        SET HIGH INTENSITY                           
         MVC   ODBH+4(2),=X'0C07'                                               
DISP6    LH    R8,DISPSB           SET DISPSB TO END OF LINE                    
         AR    R8,R6                                                            
         BCTR  R8,R0                                                            
         STH   R8,DISPSB                                                        
         TM    DISPOP,X'04'                                                     
         BZ    DISP7                                                            
         CVD   R8,DUB                                                           
         UNPK  DUB(5),DUB+5(3)                                                  
         OI    DUB+4,X'F0'                                                      
         MVC   ODB+6(5),DUB                                                     
         MVI   ODBH+7,11                                                        
         OI    ODBH+6,OI1T                                                      
*                                                                               
DISP7    TM    DISPOP,X'08'                                                     
         BZ    DISP8                                                            
         FOUT  ODCH,(R5),(R6)      MOVE IN CHARACTERS                           
*        STC   R6,ODCH+5                                                        
         L     R8,ADISPTBL                                                      
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         TR    ODC(0),0(R8)        CHANGE INV CHRS TO QUESTION MARK             
         LA    R6,1(R6)                                                         
*                                                                               
DISP8    LH    R8,DISPSB           UP DISPSB TO NEXT LINE                       
         LA    R8,1(R8)                                                         
         STH   R8,DISPSB                                                        
         LH    R8,DISPCT           DOWN RESIDUAL COUNT                          
         SR    R8,R6                                                            
         STH   R8,DISPCT                                                        
         LA    R4,L'DLINE(R4)      UP TWA POINTER                               
         AR    R5,R6               UP IOAREA POINTER                            
         BCT   R7,DISP3                                                         
*                                                                               
         OI    ODBH+6,OI1T         TRANSMIT TAB UNPROT FLD                      
         CLI   ODBH,11             END OF SCREEN                                
         BE    DISPX               YES                                          
         OI    ODHH+6,OI1T                                                      
*                                                                               
         LA    R6,L'DLINE                                                       
         MH    R6,DISPNL                                                        
         LA    R6,DLINE(R6)                                                     
         SR    R0,R0                                                            
DISP9    IC    R0,0(R6)            FIND END OF TWA                              
         LTR   R0,R0                                                            
         BZ    *+10                                                             
         AR    R6,R0                                                            
         B     DISP9                                                            
         MVC   1(2,R6),=X'0000'    SET BEFORE/AFTER     *****                   
*                                                                               
DISPX    XIT1                                                                   
         EJECT                                                                  
*        THIS ROUTINE DOES ALL DISK I/O VIA DATA MANAGER                        
*                                                                               
DISKIO   NTR1  BASE=ABASE                                                       
         XC    WRK,WRK                                                          
         LA    R1,DMCBWS                                                        
         USING DMCBD,R1                                                         
         SR    R4,R4                                                            
         IC    R4,DISKIOOP                                                      
         MH    R4,=H'3'                                                         
         CLI   STATUS,0                                                         
         BE    *+8                                                              
         LA    R4,6(R4)                                                         
         LA    R4,STIPERM+3(R4)    R4=A(I/O INFO)                               
         SR    R5,R5                                                            
         IC    R5,0(R4)            R5=COMMAND NO                                
         BCTR  R5,R0                                                            
         MH    R5,=H'6'                                                         
         L     R6,ADMCMDS                                                       
         AR    R6,R5               R6=A(COMMAND TO DATAMGR)                     
         ST    R6,DMCB1                                                         
         MVI   DMCTL1,X'09'        RETURN DELETS & REC LEN                      
*                                                                               
         CLC   0(3,R6),=C'DMR'     CLEAR IOAREA BEFORE READ                     
         BE    *+14                                                             
         CLC   0(3,R6),=C'GET'                                                  
         BNE   DIO1                                                             
         LA    RE,IOAREA                                                        
         L     RF,=A(IOAREAX-IOAREA)                                            
         XCEF                                                                   
*                                                                               
DIO1     SR    R5,R5                                                            
         IC    R5,STIFN            R5=FILE NUM                                  
         BCTR  R5,R0                                                            
         MH    R5,=H'20'                                                        
         L     R6,AFILETBL                                                      
         AR    R6,R5               R6=A(FILE NAME)                              
         ST    R6,DMCB2                                                         
*                                                                               
         CLI   1(R4),1                                                          
         BH    *+12                                                             
         LA    R6,STIK                                                          
         B     DIO2                                                             
         CLI   1(R4),2                                                          
         BNE   *+12                                                             
         LA    R6,SLIK                                                          
         B     DIO2                                                             
         CLI   1(R4),3                                                          
         BNE   *+12                                                             
         L     R6,STIK                                                          
         B     DIO2                                                             
         DC    H'0'                                                             
DIO2     ST    R6,DMCB3            R6=A(KEY OR ADR)                             
*                                                                               
         LA    R6,IOAREA                                                        
         ST    R6,DMCB4                                                         
*                                                                               
         LA    R6,IOWORK                                                        
         ST    R6,DMCB5                                                         
*                                                                               
         XC    DMCB6,DMCB6                                                      
         CLI   STIFRT,1                                                         
         BNE   *+16                                                             
         LH    R6,STIFRL           SET REC LEN FOR F/L                          
         LA    R6,1(R6)                                                         
         STH   R6,SLRL                                                          
         MVC   DMRECL(2),SLRL      SET V/L TO LAST READ LEN                     
*                                                                               
DIO2A    CLI   STIFN,2             TEST TEMPSTR                                 
         BNE   DIO2B                                                            
         MVC   DMCB6(2),=C'L='                                                  
         MVC   DMCB6+2(2),TWAL                                                  
         B     DIO3                                                             
DIO2B    CLI   STIFN,14            TEST TEMPEST                                 
         BNE   DIO3                                                             
         MVC   DMCB6(2),=C'L='                                                  
         MVC   DMCB6+2(2),TMSL                                                  
*                                                                               
DIO3     L     RF,ADATAMGR                                                      
         BASR  RE,RF                                                            
         TM    DMCTL3,X'FC'                                                     
         BC    5,DIOMISS           EOF/ERR/DKEY/NOTF/PHL/SECL                   
         CLI   STIFRLBN,X'FF'      RECORD LEN IN RECORD                         
         BE    *+20                NO                                           
         SR    R6,R6               YES SET IN DMCB                              
         IC    R6,STIFRLBN                                                      
         LA    R6,IOAREA(R6)                                                    
         MVC   DMRECL,0(R6)                                                     
*                                                                               
         CLI   STITN,0             SAVE AND STRIP RECOVERY FILE HEADER          
         BE    DIOHIT              NO                                           
         MVC   STITHDR(24),IOAREA                                               
         LH    R1,DMCBWS+22                                                     
         SH    R1,=H'24'                                                        
         STH   R1,DMCBWS+22                                                     
         LA    RF,IOAREA                                                        
         LA    RE,24(RF)                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         LH    R1,DMCBWS+22                                                     
         LA    RF,IOAREA(R1)                                                    
         XC    0(24,RF),0(RF)                                                   
         LA    R1,DMCBWS           RESTORE DMCB POINTER                         
*                                                                               
DIOHIT   SR    R6,R6               SET READ INDIC & LENGTH                      
         IC    R6,2(R4)                                                         
         LA    R6,1(R6)                                                         
         STC   R6,SLRI                                                          
DIOHL    MVC   SLRL,DMRECL                                                      
         MVC   SLRF,STIFN                                                       
         MVI   HDRN,0                                                           
         SR    R6,R6                                                            
         ICM   R6,3,SLRL                                                        
         MVC   WRK(3),=C'RL='                                                   
         EDIT  (R6),(5,WRK+3),ALIGN=LEFT                                        
*                                                                               
DIOH0    CLI   2(R4),0             SET KEY AS INSTRUCTED                        
         BE    DISKIOX                                                          
DIOH1    CLI   2(R4),1                                                          
         BNE   DIOH2                                                            
         L     R5,DMCB3                                                         
         LA    R6,4                                                             
         MVC   WRK+9(3),=C'RA='                                                 
         GOTO1 AHEXOUT,HEXWS,(R5),WRK+12,(R6),=C'MIX'                           
         B     DIOHX                                                            
DIOH2    CLI   2(R4),2                                                          
         BNE   DIOH3                                                            
         LA    R5,IOAREA                                                        
         SR    R6,R6                                                            
         IC    R6,STIFKL                                                        
         B     DIOHX                                                            
DIOH3    CLI   2(R4),3                                                          
         BNE   DIOH4                                                            
         LA    R5,STIK                                                          
         SR    R6,R6                                                            
         IC    R6,STIKL                                                         
         B     DIOHX                                                            
DIOH4    DC    H'0'                                                             
*                                                                               
DIOHX    LA    R7,SLIK             R5=A(KEY/ADR) R6=L'KEY/ADR                   
         CR    R7,R5                                                            
         BE    *+10                                                             
         XC    SLIK,SLIK                                                        
         STC   R6,SLIKL                                                         
         BCTR  R6,R0                                                            
         EX    R6,DIOHMV                                                        
         B     DISKIOX                                                          
DIOHMV   MVC   SLIK(0),0(R5)                                                    
*                                                                               
DIOMISS  MVI   SLRI,0                                                           
         TM    DMCTL3,X'40'        DISKERROR                                    
         BZ    *+12                                                             
         MVI   HDRN,1                                                           
         B     DIOMISS1                                                         
         TM    DMCTL3,X'80'        EOF                                          
         BZ    *+12                                                             
         MVI   HDRN,2                                                           
         B     DIOMISS1                                                         
         TM    DMCTL3,X'10'        NOTFOUND                                     
         BZ    *+12                                                             
         MVI   HDRN,3                                                           
         B     DIOMISS1                                                         
         TM    DMCTL3,X'2C'        DUPKEYADD/PHYSLOCK/SECLOCK                   
         BZ    *+12                                                             
         MVI   HDRN,4                                                           
         B     DIOMISS1                                                         
         DC    H'0'                                                             
DIOMISS1 EQU   *                                                                
*                                                                               
DISKIOX  XIT1                                                                   
         EJECT                                                                  
*FILETBL FOR EACH FILE THIS TABLE CONTAINS                                      
*        CL8   NAME                00=END-OF-TABLE                              
*        XL1   NUMBER                                                           
*        XL1   TYPE ORGANISATION   01=SEQ,02=IS,03=DA,04=DAL                    
*        XL1   TYPE LOGICAL        01=REQUEST                                   
*        XL1   RECORD TYPE         01=FIX,02=V/L,03=V/L/ELEMENTS                
*        XL2   MAX REC LEN - 1                                                  
*        XL1   KEY LEN                                                          
*        XL1   CONTROL LENGTH                                                   
*        XL1   SYS SAVE LEN                                                     
*        XL1   START BYTE REC LEN  FF=NOT STORED IN RECORD                      
*        XL1   1ST KEY BYTE NUM    FF=NO 1ST KEY SETTING                        
*        XL1   KEY FILL CHARACTER                                               
*                                                                               
*EXTERNAL TABLE                                                                 
         SPACE 2                                                                
*KEYTBL  FOR EACH KEY THIS TABLE CONTAINS                                       
*        CL2   FIRST TWO CHRS OF NAME                                           
*        XL1   NUMBER                                                           
*        XL1   X'01'=VALID FOR RECORD  X'02'=VALID FOR ELEMENT                  
*                                                                               
KEYTBL   DS    0CL4                                                             
         DC    C'K,',AL1(01),X'01'                                              
         DC    C'A,',AL1(02),X'01'                                              
         DC    C'FI',AL1(03),X'03'                                              
         DC    C'NE',AL1(04),X'01'                                              
         DC    C'S,',AL1(05),X'02'                                              
         DC    C'I,',AL1(06),X'02'                                              
         DC    C'LA',AL1(07),X'03'                                              
KEYTBLX  DC    X'00'                                                            
         SPACE 2                                                                
*ACTNTBL FOR EACH ACTION THIS TABLE CONTAINS                                    
*        CL7   ACTION NAME                                                      
*        XL1   NUMBER                                                           
*        XL1   X'01'=VALID FOR RECORD  X'02'=VALID FOR ELEMENT                  
*                                                                               
ACTNTBL  DS    0CL9                                                             
         DC    C'DISPLAY',AL1(01),X'03'                                         
*        DC    C'CHANGE ',AL1(02),X'03'                                         
*        DC    C'ADD    ',AL1(03),X'03'                                         
         DC    C'BROWSE ',AL1(04),X'01'                                         
*        DC    C'REPEAT ',AL1(05),X'01'                                         
         DC    C'EXTRACT',AL1(06),X'01'                                         
ACTNTBLX DC    X'00'                                                            
         EJECT                                                                  
*PERMTBL FOR EACH VALID REQUEST THIS TABLE CONTAINS                             
*        XL3   FILE NUM , KEY NUM , ACTION NUM                                  
*        XL3   STATUS 0 FIRST I/O INFO                                          
*        XL3   STATUS 0 SECND I/O INFO                                          
*        XL3   STATUS 1 FIRST I/O INFO                                          
*        XL3   STATUS 1 SECND I/O INFO                                          
*        I/O INFO CONTAINS                                                      
*        XL1   I/O COMMAND NO                                                   
*        XL1   SET UP KEY/ADR INSTRUCTION                                       
*        XL1   SAVE KEY/ADR INSTRUCTION                                         
*        SET UP VALUES                                                          
*        0     NOTHING                                                          
*        1     POINT DMCB3 TO STIK                                              
*        2     POINT DMCB3 TO SLIK                                              
*        3     SPECIAL                                                          
*        SAVE VALUES                                                            
*        0     NOTHING                                                          
*        1     SAVE ADR FROM DMCB3 (& DISPLAY IT)                               
*        2     SAVE KEY FROM RECORD                                             
*        3     SAVE KEY/ADR FROM STIK                                           
*                                                                               
*EXTERNAL TABLE                                                                 
         SPACE 2                                                                
*        THIS TABLE TRANSLATES A COMMAND NO (DMCMDN) TO A COMMAND NAME          
*                                                                               
DMCMDS   DS    0CL6                                                             
         DC    CL6'DMADD '         1                                            
         DC    CL6'DMREAD'         2                                            
         DC    CL6'DMRSEQ'         3                                            
         DC    CL6'DMRDHI'         4                                            
         DC    CL6'DMDEL '         5                                            
         DC    CL6'DMWRT '         6                                            
         DC    CL6'DMRDIR'         7                                            
         DC    CL6'ADDREC'         8                                            
         DC    CL6'PUTREC'         9                                            
         DC    CL6'GETREC'         A                                            
         EJECT                                                                  
WDISP    DC    H'20'               DISPLAY CHR WIDTH                            
NDISP    DC    H'14'               NUM OF DISPLAY LINES                         
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
*        THIS TABLE CONTAINS HEADER MSGS INDEXED BY HDRN                        
*                                                                               
OKMSGS   DS    0CL45                                                            
OK0      DC    CL45'TFM - ACTION COMPLETED'                                     
OK1      DC    CL45'TFM - ENTER NEW RECORD DATA'                                
OK2      DC    CL45'TFM - ENTER RECORD DATA CHANGES'                            
OK3      DC    CL45'TFM - ENTER ELEMENT DATA'                                   
         EJECT                                                                  
*        THIS TABLE CONTAINS ERROR MSGS INDEXED BY FERN                         
*                                                                               
ERRMSGS  DS    0CL20                                                            
ERR01    DC    CL20'MISSING FILE NAME'                                          
ERR02    DC    CL20'INVALID FILE NAME'                                          
ERR03    DC    CL20'MISSING RECORD ID'                                          
ERR04    DC    CL20'INVALID RECORD ID'                                          
ERR05    DC    CL20'SCAN SYNTAX'                                                
ERR06    DC    CL20'INVALID ELEMENT ID'                                         
ERR07    DC    CL20'INVALID HEXADECIMAL'                                        
ERR08    DC    CL20'ELEMENT NOT FOUND'                                          
ERR09    DC    CL20'DISK ADR NOT 8 CHRS'                                        
ERR10    DC    CL20'INVALID DISK ADR'                                           
ERR11    DC    CL20'NO PREV REC FOR FILE'                                       
ERR12    DC    CL20'INVALID START'                                              
ERR13    DC    CL20'START NOT NUMERIC'                                          
ERR14    DC    CL20'INVALID END'                                                
ERR15    DC    CL20'END NOT NUMERIC'                                            
ERR16    DC    CL20'START GT END'                                               
ERR17    DC    CL20'END GT REC LEN'                                             
ERR18    DC    CL20'START GT REC LEN'                                           
ERR19    DC    CL20'START MUST BE ZERO'                                         
ERR20    DC    CL20'MISSING END'                                                
ERR21    DC    CL20'END TOO BIG'                                                
ERR22    DC    CL20'END NEQ RECLEN-1'                                           
ERR23    DC    CL20'END TOO SMALL'                                              
ERR24    DC    CL20'CANT FIT ON SCREEN'                                         
ERR25    DC    CL20'INVALID ACTION NAME'                                        
ERR26    DC    CL20'ACTION NOT AVAILABLE'                                       
ERR27    DC    CL20'NOT DEFINED FOR FILE'                                       
ERR28    DC    CL20'INPUT TOO LONG'                                             
ERR29    DC    CL20'DISK ERROR'                                                 
ERR30    DC    CL20'EOF NO RECORD FOUND'                                        
ERR31    DC    CL20'RECORD NOT FOUND'                                           
ERR32    DC    CL20'CONTENTION LOCKOUT'                                         
ERR33    DC    CL20'REC ALREADY EXISTS'                                         
ERR34    DC    CL20'TERM DUE TO SIZE ERR'                                       
ERR35    DC    CL20'TERM DUE TO DISK ERR'                                       
ERR36    DC    CL20'TERM DUE TO EOF'                                            
ERR37    DC    CL20'TERM DUE TO NOTFOUND'                                       
ERR38    DC    CL20'TERM DUE TO CONTENTN'                                       
ERR39    DC    CL20'CANT CHANGE KEY'                                            
ERR40    DC    CL20'CANT CHANGE DATA'                                           
ERR41    DC    CL20'CANT CHANGE HEX&&CHR'                                       
ERR42    DC    CL20'CANT CHANGE EL LEN'                                         
ERR43    DC    CL20'INVALID EL ID CODE'                                         
ERR44    DC    CL20'KEY IS X''PP00TTTT'''                                       
         EJECT                                                                  
*        THIS TABLE DEFINES THE DISPLAYABLE CHARACTERS                          
*                                                                               
DISPTBLU DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  00-0F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  10-1F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  20-2F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  30-3F                    
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'  40-4F                    
         DC    XL16'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'  60-6F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B794B7B7C7D7E7F'  70-7F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  80-8F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  90-9F                    
         DC    XL16'4BA14B4B4B4B4B4B4B4B4B4B4B4B4B4B'  A0-AF                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  D0-D1                    
         DC    XL16'E04BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  F0-FF                    
         SPACE 2                                                                
DISPTBLL DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  00-0F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  10-1F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  20-2F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  30-3F                    
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'  40-4F                    
         DC    XL16'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'  60-6F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B794B7B7C7D7E7F'  70-7F                    
         DC    XL16'4B8182838485868788894B4B4B4B4B4B'  80-8F                    
         DC    XL16'4B9192939495969798994B4B4B4B4B4B'  90-9F                    
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B'  A0-AF                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  D0-D1                    
         DC    XL16'E04BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  F0-FF                    
         EJECT                                                                  
SYSTBL   CSECT                                                                  
         DC    X'0101'                                                          
         SPACE 2                                                                
FILETBL  CSECT                                                                  
*                                                                               
         DC    C'PRGMS   ',AL1(01,3,0,2),AL2(56663),X'000000FF',X'0300'         
         DC    C'TEMPSTR ',AL1(02,03,0,01),H'14335',X'040000FF',X'0300'         
         DC    C'ADRFILE ',AL1(03,03,00,01),H'1899',X'000000FF',X'0300'         
         DC    C'WKFILE  ',AL1(04,03,00,01),H'3659',X'000000FF',X'0300'         
         DC    C'PRTQ1   ',AL1(05,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQ2   ',AL1(06,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'STATS   ',AL1(07,03,00,01),H'2415',X'000000FF',X'0300'         
         DC    C'DMPFILE ',AL1(08,03,00,02),H'8191',X'000000FF',X'0300'         
         DC    C'TSTRCVR ',AL1(09,03,02,02),H'2047',X'000000FF',X'0300'         
         DC    C'KWXFILE ',AL1(10,03,00,03),H'0999',X'04000000',X'0300'         
         DC    C'CTFILE  ',AL1(11,02,00,03),H'0999',X'19030019',X'1800'         
         DC    C'PRTQ3   ',AL1(12,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQ4   ',AL1(13,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'TEMPEST ',AL1(14,03,0,01),H'14335',X'040000FF',X'0300'         
         DC    C'PRTQ5   ',AL1(15,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQ6   ',AL1(16,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQ7   ',AL1(17,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQ8   ',AL1(18,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'EDCTA   ',AL1(19,03,0,01),H'14335',X'000000FF',X'0300'         
         DC    C'EDCTR   ',AL1(20,03,0,01),H'14335',X'000000FF',X'0300'         
         DC    C'WRKF1   ',AL1(21,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF2   ',AL1(22,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF3   ',AL1(23,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF4   ',AL1(24,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF5   ',AL1(25,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF6   ',AL1(26,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF7   ',AL1(27,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF8   ',AL1(28,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'FACWRK  ',AL1(29,03,00,01),H'3659',X'000000FF',X'0300'         
         DC    C'EASIWK  ',AL1(30,03,00,01),H'3659',X'000000FF',X'0300'         
         DC    C'PRTQ9   ',AL1(31,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQA   ',AL1(32,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQB   ',AL1(33,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQC   ',AL1(34,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQD   ',AL1(35,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQE   ',AL1(36,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQF   ',AL1(37,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'PRTQG   ',AL1(38,03,0,01),H'13679',X'000000FF',X'0300'         
         DC    C'WRKF9   ',AL1(39,03,0,01),H'13679',X'000000FF',X'0300'         
*                                                                               
FILETBLX DC    X'00'                                                            
         SPACE 2                                                                
PERMTBL  CSECT                                                                  
*                                                                               
         DC    X'010201',X'070101000000000000000000'   PRGMS/A,/DIS             
*                                                                               
         DC    X'020101',X'020300000000000000000000'   TEMPSTR/K,/DIS           
*                                                                               
         DC    X'030201',X'070101000000000000000000'   ADRFILE/A,/DIS           
         DC    X'030204',X'070101000000000000000000'   ADRFILE/A,BRO            
         DC    X'030301',X'030101000000000000000000'   ADRFILE/FI/DIS           
         DC    X'030304',X'030101000000000000000000'   ADRFILE/FI/BRO           
*                                                                               
         DC    X'080201',X'070101000000000000000000'   DMPFILE/A,/DIS           
*                                                                               
         DC    X'040201',X'020101000000000000000000'   WKFILE/A,/DIS            
*                                                                               
         DC    X'050201',X'020101000000000000000000'   PRTQ1/A,/DIS             
         DC    X'060201',X'020101000000000000000000'   PRTQ2/A,/DIS             
*                                                                               
         DC    X'070201',X'070101000000000000000000'   STATS/A,/DIS             
         DC    X'070204',X'070101000000000000000000'   STATS/A,/BRO             
         DC    X'070304',X'030101000000000000000000'   STATS/FI/DIS             
         DC    X'070304',X'030101000000000000000000'   STATS/FI/BRO             
*                                                                               
         DC    X'090201',X'070101000000000000000000'   TSTRCVR/A,/DIS           
         DC    X'090204',X'070101000000000000000000'   TSTRCVR/A,/BRO           
         DC    X'090301',X'030101000000000000000000'   TSTRCVR/FI/DIS           
         DC    X'090304',X'030101000000000000000000'   TSTRCVR/FI/BRO           
*                                                                               
         DC    X'0A0201',X'070101000000000000000000'   KWXFILE/A,/DIS           
         DC    X'0A0204',X'070101000000000000000000'   KWXFILE/A,/BRO           
         DC    X'0A0301',X'030101000000000000000000'   KWXFILE/FI/DIS           
         DC    X'0A0304',X'030101000000000000000000'   KWXFILE/FI/BRO           
*                                                                               
         DC    X'0B0101',X'040102000000000000000000'   CTFILE/K,/DIS            
         DC    X'0B0104',X'040102000000000000000000'   CTFILE/K,/BRO            
         DC    X'0B0301',X'040102000000000000000000'   CTFILE/FI/DIS            
         DC    X'0B0304',X'040102000000000000000000'   CTFILE/FI/BRO            
*                                                                               
         DC    X'0C0201',X'020101000000000000000000'   PRTQ3/A,/DIS             
         DC    X'0D0201',X'020101000000000000000000'   PRTQ4/A,/DIS             
*                                                                               
         DC    X'0E0101',X'020300000000000000000000'   TEMPEST/K,/DIS           
         DC    X'0E0201',X'070101000000000000000000'   TEMPEST/A,/DIS           
*                                                                               
         DC    X'0F0201',X'020101000000000000000000'   PRTQ5/A,/DIS             
         DC    X'100201',X'020101000000000000000000'   PRTQ6/A,/DIS             
         DC    X'110201',X'020101000000000000000000'   PRTQ7/A,/DIS             
         DC    X'120201',X'020101000000000000000000'   PRTQ8/A,/DIS             
*                                                                               
         DC    X'130201',X'070101000000000000000000'   EDCTA/A,/DIS             
         DC    X'130204',X'070101000000000000000000'   EDCTA/A,/BRO             
         DC    X'140201',X'070101000000000000000000'   EDCTR/A,/DIS             
         DC    X'140204',X'070101000000000000000000'   EDCTR/A,/BRO             
*                                                                               
         DC    X'150201',X'020101000000000000000000'   WRKF1/A,/DIS             
         DC    X'160201',X'020101000000000000000000'   WRKF2/A,/DIS             
         DC    X'170201',X'020101000000000000000000'   WRKF3/A,/DIS             
         DC    X'180201',X'020101000000000000000000'   WRKF4/A,/DIS             
         DC    X'190201',X'020101000000000000000000'   WRKF5/A,/DIS             
         DC    X'1A0201',X'020101000000000000000000'   WRKF6/A,/DIS             
         DC    X'1B0201',X'020101000000000000000000'   WRKF7/A,/DIS             
         DC    X'1C0201',X'020101000000000000000000'   WRKF8/A,/DIS             
         DC    X'270201',X'020101000000000000000000'   WRKF9/A,/DIS             
*                                                                               
         DC    X'1D0201',X'020101000000000000000000'   FACWRK/A,/DIS            
         DC    X'1E0201',X'020101000000000000000000'   EASIWK/A,/DIS            
*                                                                               
         DC    X'1F0201',X'020101000000000000000000'   PRTQ9/A,/DIS             
         DC    X'200201',X'020101000000000000000000'   PRTQA/A,/DIS             
         DC    X'210201',X'020101000000000000000000'   PRTQB/A,/DIS             
         DC    X'220201',X'020101000000000000000000'   PRTQC/A,/DIS             
         DC    X'230201',X'020101000000000000000000'   PRTQD/A,/DIS             
         DC    X'240201',X'020101000000000000000000'   PRTQE/A,/DIS             
         DC    X'250201',X'020101000000000000000000'   PRTQF/A,/DIS             
         DC    X'260201',X'020101000000000000000000'   PRTQG/A,/DIS             
*                                                                               
PERMTBLX DC    X'00'                                                            
         SPACE 2                                                                
RCVRTBL  CSECT                                                                  
*&&US                                                                           
         DC    X'21040003',H'2000',X'0D03080D'         SPTFILE                  
         DC    X'22020001',H'0116',X'110100FF'         STATION                  
         DC    X'23020001',H'0017',X'0D0100FF'         SPTDIR                   
         DC    X'25030101',H'0105',X'020000FF'         REQUEST                  
         DC    X'2A040003',H'2000',X'14030414'         UNTFILE                  
*                                                                               
         DC    X'32040003',H'2000',X'0D03080D'         TRFFILE                  
         DC    X'33020001',H'0017',X'0D0100FF'         TRFDIR                   
*                                                                               
         DC    X'40020001',H'0030',X'190200FF'         PRTDIR                   
         DC    X'41020001',H'0030',X'190200FF'         PUBDIR                   
         DC    X'42040003',H'2000',X'19040419'         PRTFILE                  
         DC    X'43040003',H'2000',X'19040419'         PUBFILE                  
         DC    X'45030101',H'0105',X'020000FF'         PREQUES                  
*                                                                               
         DC    X'81020001',H'0031',X'1B0100FF'         REPDIR                   
         DC    X'82040003',H'2000',X'1B03041B'         REPFILE                  
         DC    X'83030101',H'0105',X'020000FF'         REPREQ                   
*                                                                               
         DC    X'71020001',H'0037',X'200200FF'         TALDIR                   
         DC    X'72040003',H'1999',X'20040420'         TALFIL                   
         DC    X'73030101',H'0959',X'020000FF'         TALREQ                   
         DC    X'75020001',H'0037',X'200200FF'         CHKDIR                   
         DC    X'76040003',H'1999',X'20040420'         CHKFIL                   
*&&                                                                             
*&&UK                                                                           
         DC    X'41020001',H'0031',X'140800FF'         MEDDIR                   
         DC    X'42040003',H'1999',X'140A0414'         MEDFIL                   
         DC    X'43030101',H'0105',X'020000FF'         REQUEST                  
*                                                                               
         DC    X'E1020001',H'0041',X'240200FF'         PERDIR                   
         DC    X'E2040003',H'1023',X'24040424'         PERFIL                   
         DC    X'E3030101',H'0105',X'020000FF'         PERREQ                   
*                                                                               
         DC    X'A8040003',H'0999',X'0E06040E'         DEMOLD                   
         DC    X'A9040003',H'0999',X'0E06040E'         DEMNEW                   
         DC    X'AA020001',H'0021',X'0E0400FF'         DEMDIR                   
*&&                                                                             
*                                                                               
         DC    X'53030101',H'0105',X'020000FF'         MPLREQ                   
         DC    X'57040003',H'1999',X'20060420'         MPQFLA                   
         DC    X'59040003',H'1999',X'20060420'         MPRFLA                   
         DC    X'5B040003',H'1999',X'20060420'         BUDFIL                   
*                                                                               
         DC    X'61020003',H'1999',X'2A03042A'         ACCOUNT                  
         DC    X'65030101',H'0105',X'020000FF'         ACCREQ                   
         DC    X'66030003',H'1999',X'02000000'         ACCDAY                   
         DC    X'69020001',H'0053',X'2A0800FF'         ACCDIR                   
         DC    X'6A040003',H'1999',X'2A0A042A'         ACCMST                   
*                                                                               
         DC    X'A1020003',H'0999',X'19030019'         CTFILE                   
         DC    X'A3030101',H'0105',X'020000FF'         CTREQ                    
         DC    X'AE020001',H'0039',X'200400FF'         GENDIR                   
         DC    X'AF040003',H'1999',X'20060420'         GENFIL                   
*                                                                               
RCVRTBLX DC    X'00'                                                            
         EJECT                                                                  
*                        CALLOV PARAMETER LIST DSECT                            
*                                                                               
CALLOVD  DSECT                                                                  
OVPARM1  DS    0F                                                               
OVNO     DS    CL1       OVERLAY NUMBER                                         
OVLOADA  DS    CL3       LOAD ADDR                                              
OVPARM2  DS    0F                                                               
OVERR    DS    CL1       ERROR CODE                                             
OVTWAA   DS    CL3       A(TWA)                                                 
         SPACE 2                                                                
*                        HEXOUT PARAMETER LIST DSECT                            
*                                                                               
HEXOUTD  DSECT                                                                  
HOAS     DS    F         A(SOURCE)                                              
HOAD     DS    F         A(DESTN)                                               
HOLS     DS    F         L'SOURCE                                               
HOAO     DS    F         A(OPTION)                                              
HOLD     DS    F         L'DESTN                                                
         SPACE 2                                                                
*FASYSFAC                                                                       
*FASSB                                                                          
*DDCOMFACS                                                                      
*DDDMCB                                                                         
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDDMCB                                                         
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SRPFMSAVE                                                      
       ++INCLUDE SRPFMTEMP                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SRPFM00   05/01/02'                                      
         END                                                                    
