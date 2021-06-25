*          DATA SET DDSCROLL   AT LEVEL 007 AS OF 05/01/02                      
*PHASE T00AA1                                                                   
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DDSCROLL                                             *         
*                                                                     *         
*  CALLED FROM:  APPLICATION PROGRAMS                                 *         
*                                                                     *         
*  INPUTS:       A(SCROLLD)                                           *         
*                                                                     *         
*  OUTPUTS:      SAME                                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- TSARD                                          *         
*                R8 -- SDPANELD                                       *         
*                R9 -- SCROLLD                                        *         
*                RA -- SECOND BASE                                    *         
*                RB -- FIRST BASE                                     *         
*                RC -- WORKING STORAGE                                *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDSCROLL - T00AA1'                                              
SCROLLER CSECT                                                                  
         PRINT NOGEN                                                            
         DS    (4*4096)X           SO WE DON'T KEEP FILLING UP THE HOLE         
         ORG   *-(4*4096)                                                       
*                                                                               
         NMOD1 WORKDX-WORKD,**SCRL**,RA,CLEAR=YES                               
         USING WORKD,RC,R2                                                      
         LA    R2,1(RC)                                                         
         LA    R2,4095(R2)                                                      
*                                                                               
         L     R9,0(R1)            A(SCROLLD)                                   
         USING SCROLLD,R9                                                       
         LA    R7,TSARBLK          A(TSAR BLOCK)                                
         USING TSARD,R7                                                         
         USING SDPANELD,R8                                                      
         DROP  R2                                                               
*                                                                               
         ST    RD,SAVERD                                                        
         MVC   SDUSERRD,4(RD)      SAVE USER'S RD                               
*                                                                               
         BAS   RE,INIT             INITIALIZE                                   
         BAS   RE,WHATSUP          MAJOR ALGORITHM                              
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
INIT     NTR1                                                                   
*                                                                               
         MVC   0(8,R9),=C'*SCROLD*'                                             
*                                                                               
         L     RF,SDCOMFCS         A(COMFACS)                                   
         MVC   DATAMGR,CDATAMGR-COMFACSD(RF)                                    
         MVC   MINIO,CMINIO-COMFACSD(RF)                                        
         MVC   CALLOV,CCALLOV-COMFACSD(RF)                                      
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A5D'                                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   TSAR,DMCB           A(TSAR)                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
WHATSUP  NTR1                                                                   
*                                                                               
* MAJOR ALGORITHM                                                               
*                                                                               
         MVI   SDWHY,SDYFRSTQ      SET 'FIRST TIME' FOR APPLICATION             
         BAS   RE,GOHOOK           HOOK TO USER                                 
         CLI   SDRETURN,X'FE'      SPECIAL -- TESTING ONLY?                     
         BNE   *+16                                                             
         MVI   TESTONLY,C'Y'       YES                                          
         MVI   SDRETURN,SDRETOK    RESET ERROR BYTE                             
         B     WHUP10                                                           
*                                                                               
         BAS   RE,TSARINIT         INITIALIZE TSAR                              
*                                                                               
         BAS   RE,READPAN          READ PANEL TABLE USING TSAR                  
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         CLC   SDPPNLEL,SDPNLKEY   IS THIS A NEW PANEL?                         
         BE    WHUP20              NO                                           
         MVI   WRTPANEL,C'Y'       WE WILL SAVE THE PANEL TABLE                 
*                                                                               
WHUP10   BAS   RE,BPANTAB          BUILD PANEL TABLE                            
*                                                                               
WHUP20   CLI   SDPANTYP,C'S'                                                    
         BNE   *+12                                                             
         BAS   RE,BLDSTWA          BUILD SINGLE TWA SCREEN                      
         B     *+8                                                              
         BAS   RE,BLDMTWA          BUILD MULTIPLE TWA SCREEN                    
*                                                                               
         CLI   WRTPANEL,C'Y'       SHOULD WE SAVE THE PANEL?                    
         BNE   *+8                 NO                                           
         BAS   RE,WRITEPAN         WRITE PANEL TABLE USING TSAR                 
         CLI   SDACTION,SDACTPAN   BUILD/DISPLAY THE PANEL ONLY?                
         BE    WHUPX               YES                                          
*                                                                               
         BAS   RE,FILLDTYP         GIVE USER RELOCATED ADCONS                   
         MVI   SDWHY,SDYRECQ       TELL APPLICATION TO READ A RECORD            
         MVI   SDRETURN,SDRETOK                                                 
         BAS   RE,GOHOOK           HOOK TO USER                                 
         CLI   SDRETURN,SDRETOK                                                 
         BNE   WHUPX                                                            
*                                                                               
         CLI   SDACTION,SDACTVAL   VALIDATE ALL FIELDS?                         
         BNE   WHUP30              NO                                           
         CLI   SDPANTYP,C'S'                                                    
         BE    *+6                                                              
         DC    H'0'                NOT DOING LIST RIGHT NOW                     
         BAS   RE,UPDATE           UPDATE ONE RECORD                            
         MVI   SDWHY,SDYBLDQ       TELL APPLICATION TO BUILD THE RECORD         
         MVI   SDRETURN,SDRETOK                                                 
         BAS   RE,GOHOOK           HOOK TO USER                                 
         B     WHUPX                                                            
*                                                                               
WHUP30   CLI   SDACTION,SDACTDIS   DISPLAY ALL FIELDS?                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SDPANTYP,C'S'                                                    
         BNE   *+12                                                             
         BAS   RE,DISPLAY          DISPLAY ONE RECORD                           
         B     *+8                                                              
         BAS   RE,LIST             LIST RECORDS                                 
*                                                                               
WHUPX    B     XIT                                                              
         EJECT                                                                  
* BUILD PANEL TABLE                                                             
*                                                                               
BPANTAB  NTR1  WORK=(R2,BPWORKDX-BPWORKD)                                       
         USING BPWORKD,R2                                                       
*                                                                               
         LA    R8,PANELTAB                                                      
         MVC   0(8,R8),=C'*PNLTAB*'                                             
         MVC   SDPPNLEL,SDPNLKEY   SAVE PANEL KEY                               
         LA    R8,4*SDPLENQ(R8)    POINT TO FIRST ENTRY IN PANEL TABLE          
*                                                                               
         LA    R6,MINBLOCK                                                      
         USING MINBLKD,R6                                                       
         LA    RE,MINBLOCK         CLEAR MINBLOCK                               
         LA    RF,MINBLKL                                                       
         XCEFL                                                                  
*                                                                               
         MVC   MINFIL,=C'GENFIL  ' FILE NAME                                    
         MVC   MINDIR,=C'GENDIR  ' DIR NAME                                     
         MVI   MINRDUP,C'N'        DON'T READ FOR UPDATE                        
         MVI   MINFKLEN,PNLKLENQ   KEY LENGTH                                   
         MVI   MINNCTL,L'PNLKSTAT  NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(1976) MAXIMUM RECORD LENGTH                        
         MVI   MINEKLEN,L'PNLKELEM ELEMENT KEY LENGTH                           
         MVI   MINEKDSP,L'PNLKMAST ELEMENT KEY DISPLACEMENT                     
         LA    RF,MINBUFFR                                                      
         ST    RF,MINBUFF          A(FIRST BUFFER)                              
         MVI   MINNBUF,1           USE ONE BUFFER                               
         LA    RF,MINRECTB                                                      
         ST    RF,MINRTAB          A(RECORD TABLE)                              
         MVC   MINRTABL,=AL2(L'MINRECTB)  LENGTH OF RECORD TABLE                
         LA    RF,MINELEMN                                                      
         ST    RF,MINELEM          A(ELEMENT AREA)                              
         MVC   MINMAXEL,=AL2(L'MINELEMN)  MAX LENGTH OF ELEMENT                 
         XC    MINELEMN,MINELEMN                                                
         MVI   MINFILTL,1          FILTER ON ELEMENT CODE ONLY                  
         MVC   MINCOMF,SDCOMFCS    A(COMFACS)                                   
         MVC   MINRECUP,SDRECUP    A(RECUP)                                     
*                                                                               
         LA    R4,MINMKEY                                                       
         XC    MINMKEY,MINMKEY     CLEAR MASTER KEY FOR MINIO                   
         USING PNLKEYD,R4                                                       
         MVI   PNLKSYS,PNLKSYSQ    PANEL RECORD TYPE                            
         MVI   PNLKSTYP,PNLKSTYQ                                                
         MVC   PNLKSYPG,SDSYSPRG   SYSTEM/PROGRAM                               
         MVC   PNLKNAME,SDPANEL    PANEL NAME                                   
         MVC   PNLKAGY,SDAGENCY    AGENCY CODE (POWER CODE)                     
         MVC   PNLKMED,SDMEDIA     MEDIA CODE                                   
         MVC   PNLKCLT,SDCLIENT    CLIENT CODE                                  
         MVC   PNLKTYPE,SDPANTYP   PANEL TYPE (SINGLE/MULTIPLE)                 
         MVI   MINEKEY,PNLELTCQ    LOOK FOR DATATYPE ELEMENTS                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINHI',(R6))                                        
         CLI   MINERR,MINESNF      WAS CLIENT-SPECIFIC PANEL FOUND?             
         BNE   BP10                YES                                          
*                                                                               
         XC    PNLKCLT,PNLKCLT                                                  
         GOTO1 (RF),(R1)           GOTO MINIO -- PARAM LIST IS SET              
         CLI   MINERR,MINESNF      WAS MEDIA-SPECIFIC PANEL FOUND?              
         BNE   BP10                YES                                          
*                                                                               
         XC    PNLKMED,PNLKMED                                                  
         GOTO1 (RF),(R1)           GOTO MINIO -- PARAM LIST IS SET              
         CLI   MINERR,MINESNF      WAS AGENCY-SPECIFIC PANEL FOUND?             
         BNE   BP10                YES                                          
*                                                                               
         XC    PNLKAGY,PNLKAGY                                                  
         GOTO1 (RF),(R1)           GOTO MINIO -- PARAM LIST IS SET              
         CLI   MINERR,MINESNF      WAS GENERIC PANEL FOUND?                     
         BNE   BP10                YES                                          
         MVI   SDRETURN,SDRETNOP   NO -- PANEL RECORD ISN'T THERE               
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BP10     MVC   PANELKEY,MINMKEY    SAVE PANEL RECORD KEY                        
         LA    R5,MINELEMN                                                      
         USING PNLELTD,R5                                                       
*                                                                               
BP20     CLI   MINERR,0            ANY ERROR?                                   
         BE    *+14                NO -- PROCESS THE DATATYPE                   
         CLI   MINERR,MINEEOF      DATATYPE ELEMENT FOUND?                      
         BE    BP60                                                             
         DC    H'0'                DIE ON OTHER MINIO ERROR                     
*                                                                               
         CLC   =C'USER',PNLEDTYP   IS IT USER FIELD?                            
         BNE   BP30                NO, PROCESS DATATYPE RECORDS                 
*                                                                               
         LA    RF,PANELTAB         INCREMENT NUMBER OF USER LINES               
         ZIC   RE,SDPNUMUS                                                      
         LA    RE,1(RE)                                                         
         STC   RE,SDPNUMUS-SDPANELD(RF)                                         
         BAS   RE,BPUELEM          EXTRACT DATA FROM ELEMENT                    
         B     BP50                                                             
*                                                                               
BP30     CLC   =C'SEL',PNLEDTYP    IS THIS THE RESERVED 'SEL' FIELD?            
         BE    BP40                                                             
         CLC   =C'KEY',PNLEDTYP    IS THIS A RESERVED 'KEY' FIELD?              
         BE    BP40                                                             
         CLC   =C'TEXT   ',PNLEDTYP IS THIS A RESERVED 'TEXT' FIELD?            
         BNE   *+12                                                             
BP40     BAS   RE,BLDPNENS         BUILD SPECIAL PANEL TABLE ENTRY              
         B     BP50                                                             
*                                                                               
         LA    R3,KEY              BUILD KEY FOR DATATYPE RECORD                
         USING DTYPKEYD,R3                                                      
         XC    KEY,KEY                                                          
         MVI   DTYPSYS,DTYPSYSQ    SYSTEM ID X'00'                              
         MVI   DTYPTYP,DTYPTYPQ    RECORD TYPE X'0D'                            
         MVC   DTYPSYPG,PNLKSYPG   SYSTEM/PROGRAM                               
         MVC   DTYPCODE,PNLEDTYP   DATATYPE CODE                                
         DROP  R5                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR  ',KEY,KEY                 
         CLI   DMCB+8,0            ANY ERROR?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL  ',DTYPDA,DTYPEIO,+        
               DMWORK                                                           
         CLI   DMCB+8,0            ANY ERROR?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         LR    R1,R5                                                            
         BAS   RE,BLDPNENT         BUILD PANEL TABLE ENTRY FOR DATATYPE         
*                                                                               
BP50     LA    R5,MINELEMN                                                      
         GOTO1 MINIO,DMCB,('MINSEQ',(R6))                                       
         B     BP20                                                             
         DROP  R4                                                               
*                                                                               
BP60     LA    R8,PANELTAB         A(PANEL TABLE)                               
         MVI   MINEKEY,PNLPFDEQ    LOOK FOR PFKEY TEXT ELEMENTS                 
         GOTO1 MINIO,DMCB,('MINHI',(R6))                                        
*                                                                               
BP70     CLI   MINERR,MINEEOF      END-OF-FILE?                                 
         BE    BP80                YES                                          
         CLI   MINERR,0            PFKEY ELEMENT FOUND?                         
         BE    *+6                 YES                                          
         DC    H'0'                DIE ON OTHER MINIO ERROR                     
*                                                                               
         USING PNLPFD,R5                                                        
         ZIC   RE,PNLPFNUM         PFKEY NUMBER                                 
         BCTR  RE,0                                                             
         MH    RE,=H'8'                                                         
         LA    RE,SDPFKEYS(RE)     INDEX INTO PFKEY TABLE                       
         MVC   0(8,RE),PNLPFTXT    PFKEY TEXT                                   
         DROP  R5                                                               
*                                                                               
         GOTO1 MINIO,DMCB,('MINSEQ',(R6))                                       
         B     BP70                                                             
         DROP  R6                                                               
*                                                                               
BP80     SR    R1,R1                                                            
         BAS   RE,BUILDPFS         COUNT NUMBER OF PFKEY LINES NEEDED           
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 3                                                                
BPWORKD  DSECT                                                                  
MINBLOCK DS    (MINBLKL)X          MINIO PARAMETER BLOCK AREA                   
MINBUFFR DS    2000X               MINIO BUFFER FOR PANEL RECORDS               
MINRECTB DS    XL100               MINIO RECORD TABLE                           
MINELEMN DS    XL100               MINIO ELEMENT AREA                           
BPWORKDX EQU   *                                                                
         SPACE 3                                                                
SCROLLER CSECT                                                                  
         EJECT                                                                  
* BUILD A PANEL TABLE ENTRY FOR A DATATYPE.  WE BEGIN WITH THE DEFAULTS         
* GIVEN IN THE DATATYPE RECORD, THEN REPLACE THESE WITH ANY OVERRIDES           
* AND OTHER DATA FOUND IN THE PANEL RECORD ELEMENT FOR THAT DATATYPE.           
*                                                                               
* R1 = A(PANEL RECORD ELEMENT)                                                  
*                                                                               
BLDPNENT NTR1                                                                   
*                                                                               
         ST    R1,FULL             A(PANEL RECORD ELEMENT)                      
         LA    R5,DTYPEIO          A(DATATYPE RECORD)                           
         USING DTGEND,R5           GENERAL INFO ELEMENT                         
         MVI   ELCODE,DTGENELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SDPIDNUM,DTGENIDN   ID NUMBER                                    
         MVC   SDPDATLN,DTGENOSL   ON-SCREEN LENGTH                             
         MVC   SDPTABLN,DTGENDTL   DATA TABLE LENGTH                            
         CLI   DTGENDUL,C'L'       UPPER/LOWER CASE FIELD?                      
         BNE   *+8                                                              
         MVI   SDPATTR,X'40'       SET STANDARD ATTRIBUTE BYTE                  
         DROP  R5                                                               
*                                                                               
         CLI   SDPANTYP,C'S'       SINGLE RECORD DISPLAY?                       
         BNE   BPN10               NO -- MUST BE MULTIPLE                       
         LA    R5,DTYPEIO          A(DATATYPE RECORD)                           
         USING DTNMRD,R5           NAME TEXT ELEMENT (ROW DISPLAY)              
         MVI   ELCODE,DTNMRELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   BPN20                                                            
         MVC   SDPROWNM,DTNMRTXT   NAME TEXT                                    
         MVC   SDPROWNL,DTNMRLEN   NAME TEXT LENGTH                             
         MVC   SDPNAMLN,DTNMRLEN                                                
         B     BPN20                                                            
         DROP  R5                                                               
*                                                                               
BPN10    LA    R5,DTYPEIO          A(DATATYPE RECORD)                           
         USING DTNMCD,R5           NAME TEXT ELEMENT (COLUMN DISPLAY)           
         MVI   ELCODE,DTNMCELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   BPN20                                                            
*                                                                               
         MVC   SDPCL1NL,DTNMCLN1   TOP NAME LENGTH                              
         MVC   SDPCL1NM,DTNMCTX1   TOP NAME TEXT                                
         MVC   SDPCL2NL,DTNMCLN2   BOTTOM NAME LENGTH                           
         MVC   SDPCL2NM,DTNMCTX2   BOTTOM NAME TEXT                             
         DROP  R5                                                               
*                                                                               
BPN20    L     R5,FULL             A(PANEL FIELD ELEMENT)                       
         USING PNLELTD,R5                                                       
*                                                                               
         MVC   SDPDTYPE,PNLEDTYP   DATATYPE RECORD NAME                         
         MVC   SDPREPID,PNLERPID   REPETITION ID CODE                           
         MVC   SDPREPID,PNLERPID   REPETITION ID CODE                           
         MVC   SDPROW,PNLEROW      RELATIVE ROW NUMBER                          
         MVC   SDPCOL,PNLECOL      COLUMN NUMBER                                
         MVC   SDPSUBPN,PNLESUBP   SUB-PANEL CODE                               
         TM    PNLEFLAG,PNLEFIXQ                                                
         BZ    *+8                                                              
         OI    SDPFLAGS,SDPFIXED   FIXED FIELD (DOES NOT SCROLL)                
         TM    PNLEFLAG,PNLEBELO                                                
         BZ    *+8                                                              
         OI    SDPFLAGS,SDPBELOW   FIELD GOES BELOW OTHERS ON MULTIPLE          
*                                                                               
         TM    PNLEDEFA,PNLEIDNM   ANY OVERRIDE ID NUMBER?                      
         BO    *+10                NO                                           
         MVC   SDPIDNUM,PNLEIDNO                                                
*                                                                               
         MVC   BYTE,PNLEATTR       STANDARD ATTRIBUTE BYTE                      
         NI    BYTE,X'FF'-X'40'    WITHOUT LOWER CASE INDICATOR                 
         OC    SDPATTR,BYTE                                                     
         TM    PNLEDEFA,PNLEUPLO   ANY OVERRIDE CASE?                           
         BO    *+20                NO                                           
         NI    SDPATTR,X'FF'-X'40' ASSUME UPPER ONLY                            
         TM    PNLEATTR,X'40'      UPPER/LOWER?                                 
         BZ    *+8                                                              
         OI    SDPATTR,X'40'       YES                                          
*                                                                               
         CLI   SDPDATLN,X'FF'      SPECIAL VALUE MEANING ZERO                   
         BNE   *+12                                                             
         MVI   SDPDATLN,0                                                       
         B     BPN40                                                            
*                                                                               
         MVC   SDPDDISP,SDDDTALN   UPDATE DATATABLE RECORD LENGTH               
         ZIC   RF,SDPTABLN                                                      
         AH    RF,SDDDTALN                                                      
         STH   RF,SDDDTALN                                                      
*                                                                               
         L     RF,SDDTYPES         A(DATATYPE TABLE)                            
         LTR   R0,RF                                                            
         BZ    BPN40               NO DATATYPE TABLE GIVEN                      
         AH    R0,SDDTASIZ         A(END OF DATATYPE TABLE)                     
         USING SDDTYPED,RF                                                      
         LA    RE,1                                                             
BPN30    CLC   SDPDKEY,SDDDKEY     FIND MATCH ON DATATYPE/REPETITION ID         
         BNE   *+12                                                             
         STC   RE,SDPDTNUM                                                      
         B     BPN40                                                            
*                                                                               
         LA    RE,1(RE)            TRY NEXT ONE                                 
         LA    RF,SDDDTPLQ(RF)                                                  
         CR    RF,R0               END OF TABLE?                                
         BL    BPN30               YES                                          
         MVI   SDRETURN,SDRETNOD   NO MATCHING ENTRY IN DATATYPE TABLE          
         B     ERREX               TAKE ERROR EXIT                              
         DROP  RF                                                               
*                                                                               
BPN40    CLI   SDPANTYP,C'S'       SINGLE-RECORD DISPLAY?                       
         BNE   BPN50               NO -- MUST BE MULTIPLE                       
         CLI   PNLERWNL,0          ANY OVERRIDE NAME GIVEN?                     
         BE    *+22                NO                                           
         MVC   SDPROWNM,PNLERWNM   ROW OVERRIDE NAME                            
         MVC   SDPROWNL,PNLERWNL   L'NAME                                       
         MVC   SDPNAMLN,PNLERWNL                                                
         CLI   PNLEFLEN,0          ANY OVERRIDE NAME LENGTH?                    
         BE    *+10                NO                                           
         MVC   SDPNAMLN,PNLEFLEN   NAME FIELD LENGTH                            
         CLI   SDPNAMLN,X'FF'      SPECIAL VALUE MEANING ZERO                   
         BNE   *+8                                                              
         MVI   SDPNAMLN,0                                                       
         B     BPN60                                                            
*                                                                               
BPN50    CLI   PNLECL1L,0          ANY OVERRIDE TOP NAME GIVEN?                 
         BE    *+16                NO                                           
         MVC   SDPCL1NM,PNLECL1N   COLUMN OVERRIDE NAME (TOP)                   
         MVC   SDPCL1NL,PNLECL1L   L'NAME (TOP)                                 
         CLI   PNLECL2L,0          ANY OVERRIDE BOTTOM NAME GIVEN?              
         BE    *+16                NO                                           
         MVC   SDPCL2NM,PNLECL2N   COLUMN OVERRIDE NAME (BOTTOM)                
         MVC   SDPCL2NL,PNLECL2L   L'NAME (BOTTOM)                              
*                                                                               
         ZIC   RE,SDPCL1NL                                                      
         ZIC   RF,SDPCL2NL                                                      
         CR    RE,RF                                                            
         BL    *+6                                                              
         LR    RF,RE                                                            
         STC   RF,SDPNAMLN         RF = WIDER OF TOP AND BOTTOM                 
*                                                                               
BPN60    LA    RF,PANELTAB                                                      
         AH    RF,=Y(PANELTLQ)     RF = A(END OF PANEL TABLE)                   
         CR    R8,RF               ENOUGH ROOM FOR THIS PANEL ENTRY?            
         BNH   *+6                                                              
         DC    H'0'                MUST MAKE PANEL TABLE LARGER                 
*                                                                               
         LA    R8,SDPLENQ(R8)      UPDATE A(CURRENT PANEL TABLE ENTRY)          
         MVI   0(R8),X'FF'         MARK END OF TABLE                            
*                                                                               
XIT8     XIT1  REGS=(R8)                                                        
         DROP  R5                                                               
         EJECT                                                                  
* BUILD A PANEL TABLE ENTRY FOR A SPECIAL DATATYPE,                             
* E.G., 'SEL', 'KEY', OR 'TEXT'.                                                
*                                                                               
BLDPNENS NTR1                                                                   
*                                                                               
         USING PNLELTD,R5                                                       
*                                                                               
         MVC   SDPDTYPE,PNLEDTYP   DATATYPE RECORD NAME                         
         MVC   SDPATTR,PNLEATTR    STANDARD ATTRIBUTE BYTE                      
         TM    PNLEFLAG,PNLEFIXQ                                                
         BZ    *+8                                                              
         OI    SDPFLAGS,SDPFIXED   FIXED FIELD (DOES NOT SCROLL)                
         MVC   SDPDATLN,PNLEDLEN   DATA FIELD LENGTH                            
*                                                                               
         LA    RE,6                LENGTH OF 'SELECT'                           
         CLC   =C'SEL',PNLEDTYP                                                 
         BE    BPS10                                                            
         LA    RE,3                LENGTH OF 'KEY'                              
         CLC   =C'KEY',PNLEDTYP                                                 
         BE    BPS10                                                            
         CLC   =C'TEXT   ',PNLEDTYP                                             
         BE    BPS20                                                            
         DC    H'0'                                                             
*                                                                               
BPS10    ZIC   R1,SDPDATLN         'NAME' IS DATATYPE NAME                      
         CR    R1,RE                                                            
         BNH   *+6                                                              
         LR    R1,RE                                                            
         STC   R1,SDPCL1NL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SDPCL1NM(0),PNLEDTYP                                             
         B     BPS30                                                            
*                                                                               
BPS20    MVC   SDPROW,PNLEROW      RELATIVE ROW NUMBER                          
         MVC   SDPCOL,PNLECOL      COLUMN NUMBER                                
         MVC   SDPSUBPN,PNLESUBP   SUB-PANEL CODE                               
         CLI   PNLERWNL,0          ANY OVERRIDE NAME GIVEN?                     
         BE    BPS30               NO                                           
         MVC   SDPROWNM,PNLERWNM   ROW OVERRIDE NAME                            
         MVC   SDPROWNL,PNLERWNL   L'NAME                                       
         MVC   SDPNAMLN,PNLERWNL                                                
         DROP  R5                                                               
*                                                                               
BPS30    LA    RF,PANELTAB                                                      
         AH    RF,=Y(PANELTLQ)     RF = A(END OF PANEL TABLE)                   
         CR    R8,RF               ENOUGH ROOM FOR THIS PANEL ENTRY?            
         BNH   *+6                                                              
         DC    H'0'                MUST MAKE PANEL TABLE LARGER                 
*                                                                               
         LA    R8,SDPLENQ(R8)      UPDATE A(CURRENT PANEL TABLE ENTRY)          
         MVI   0(R8),X'FF'         MARK END OF TABLE                            
*                                                                               
         B     XIT8                                                             
         EJECT                                                                  
* CONSTRUCT PFKEY LINES FOR BOTTOM OF SCREEN.                                   
*                                                                               
*   IF R1 = 0, THEN JUST COUNT THE NUMBER OF LINES NEEDED                       
*   IF R1 > 0, THEN R1 = A(WITHIN TWA) TO BUILD TEXT LINES                      
*                                                                               
BUILDPFS NTR1  WORK=(R5,40)        GET 320 BYTES FOR PFKEY BUILD AREA           
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LR    R4,R1                                                            
*                                                                               
         LR    RE,R5               BUILD PFKEYS IN TEMPORARY AREA               
         LA    RF,320              CLEAR AREA FIRST                             
         XCEFL                                                                  
         LR    R2,R5                                                            
         LA    R1,1                24 PFKEYS                                    
         LA    RE,SDPFKEYS         PFKEY TABLE WE'VE ALREADY BUILT              
*                                                                               
BPF30    OC    0(8,RE),0(RE)       ANY TEXT FOR THIS PFKEY?                     
         BZ    BPF40                                                            
         CVD   R1,DUB              YES -- PFKEY NUMBER                          
         OI    DUB+7,X'0F'                                                      
         CH    R1,=H'9'                                                         
         BH    *+18                                                             
         UNPK  0(1,R2),DUB                                                      
         LA    R2,1(R2)                                                         
         B     *+14                                                             
         UNPK  0(2,R2),DUB                                                      
         LA    R2,2(R2)                                                         
         MVI   0(R2),C'='                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(8,R2),0(RE)       PUT TEXT INTO BLOCK                          
         MVC   8(3,R2),=X'40FF40'  DELIMIT EACH PIECE OF TEXT                   
         LA    R2,11(R2)                                                        
*                                                                               
BPF40    LA    RE,8(RE)            BUMP TO NEXT PFKEY                           
         LA    R1,1(R1)                                                         
         CH    R1,=H'24'           ANY MORE PFKEYS TO CHECK?                    
         BNH   BPF30               YES                                          
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A0D'                                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             A(SQUASHER)                                  
         GOTO1 (RF),DMCB,(R5),320  SQUASH THE TEXT                              
         ICM   R1,15,DMCB+4        LENGTH OF SQUASHED TEXT                      
         BZ    BPFX                THERE ARE NO PFKEY INSTRUCTIONS              
*                                                                               
         AR    R1,R5               R1 = A(END OF PFKEY TEXT)                    
         ST    R1,FULL                                                          
         LR    R2,R5               R2 = A(BEGINNING OF PFKEY TEXT)              
         MVI   BYTE,0              BYTE = NUMBER OF PKFEY LINES NEEDED          
*                                                                               
         LTR   R4,R4               ARE WE BUILDING THE FIELDS?                  
         BZ    BPF50                                                            
         LA    R3,DMCB             YES -- BUILD TWABLD PARAMETER LIST           
         USING TWAPARMD,R3                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD                                      
         MVC   TWAPATWA,SDTWA      A(TWA)                                       
         ST    R4,TWAPAOUT         A(OUTPUT AREA)                               
*                                                                               
         LA    RF,TWAELEM                                                       
         ST    RF,TWAPAFST         A(BUILD ELEMENT)                             
         USING TWAELEMD,RF                                                      
         XC    TWAELEM,TWAELEM     BUILD ELEMENT                                
         MVI   TWAELCD,1           ONLY ONE ELEMENT AT A TIME                   
         MVI   TWAELLN,TWAELLNQ                                                 
         LA    RE,PANELTAB                                                      
         MVC   TWAERLN,SDPFSTPF-SDPANELD(RE)   FIRST PFKEY LINE NUMBER          
         OI    TWAERLN,X'80'       THIS IS AN ABSOLUTE LINE NUMBER              
         MVI   TWAEFLN,78          ALL LINES ARE 78 BYTES LONG, . . .           
         MVI   TWAEATB,X'60'       . . .PROTECTED AND LOWER CASE                
         DROP  RF                                                               
*                                                                               
BPF50    LA    R0,1                COUNT CHARACTERS IN THIS LINE                
         XC    WORK,WORK                                                        
         MVC   WORK(2),=C'PF'                                                   
         LA    R1,WORK+2           BUILD TEXT HERE                              
*                                                                               
BPF60    LR    RE,R2                                                            
         LA    RE,1(RE)            BUMP TO NEXT CHARACTER                       
         CLI   0(RE),X'FF'         END OF THIS PFKEY'S TEXT?                    
         BNE   *-8                                                              
         SR    RE,R2                                                            
         AR    R0,RE                                                            
         CH    R0,=H'78'           DOES IT FIT ON THIS PFKEY LINE?              
         BNH   BPF80               YES                                          
*                                                                               
         ZIC   RF,BYTE             NO -- INCREMENT PFKEY LINES NEEDED           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
*                                                                               
         LTR   R4,R4               ARE WE BUILDING THE FIELDS?                  
         BZ    BPF50                                                            
         GOTO1 SDTWABLD,(R3)       YES                                          
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BPF70                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BPF70    L     RE,TWAPAOUT                                                      
         MVC   8(78,RE),WORK       PUT TEXT INTO TWA                            
         MVC   TWAPAOUT,TWAPANXT                                                
         LA    RF,TWAELEM                                                       
         MVI   TWAERLN-TWAELEMD(RF),1  NEXT LINE IS ONE ROW DOWN                
         B     BPF50                                                            
*                                                                               
BPF80    BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)       PUT TEXT INTO WORK                           
         LA    R1,1(RE,R1)         BUMP PAST TEXT AND ONE BLANK                 
         LA    R2,3(RE,R2)         YES -- BUMP TO NEXT PFKEY TEXT               
         C     R2,FULL             HAVE WE GONE BEYOND END OF TEXT?             
         BNH   BPF60                                                            
*                                                                               
         ZIC   RF,BYTE             NUMBER OF PF LINES NEEDED MINUS ONE          
         LA    R0,24               NUMBER OF LINES ON SCREEN                    
         SR    R0,RF               R0 = ABSOLUTE ROW NUMBER OF FIRST PF         
         LA    RE,PANELTAB                                                      
         STC   R0,SDPFSTPF-SDPANELD(RE)                                         
*                                                                               
         LTR   R4,R4               ARE WE BUILDING THE FIELDS?                  
         BZ    BPFX                                                             
         GOTO1 SDTWABLD,(R3)       YES                                          
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BPF90                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BPF90    L     RE,TWAPAOUT                                                      
         MVC   8(78,RE),WORK       PUT TEXT INTO TWA                            
*                                                                               
BPFX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* EXTRACT DATA FROM USER ELEMENT                                                
*                                                                               
BPUELEM  NTR1                                                                   
*                                                                               
         LR    R3,R5               A(CURRENT FIELD ELEMENT)                     
         USING PNLELTD,R3                                                       
*                                                                               
* MAP DATA FROM FIELD ELEMENT TO PANEL TABLE                                    
*                                                                               
         MVC   SDPDTYPE,PNLEDTYP   DATATYPE RECORD NAME                         
         MVC   SDPROW,PNLEROW                                                   
         MVC   SDPCOL,PNLECOL                                                   
         MVC   SDPATTR,PNLEATTR                                                 
*                                                                               
         MVI   SDPDATLN,79                                                      
         CLI   PNLEFLEN,0          IF ZERO, THEN LEN = 79                       
         BE    *+10                                                             
         MVC   SDPDATLN,PNLEFLEN   DATA FIELD LEN <- NAME FIELD LEN             
*                                                                               
         LA    RF,PANELTAB                                                      
         AH    RF,=Y(PANELTLQ)     RF = A(END OF PANEL TABLE)                   
         CR    R8,RF               ENOUGH ROOM FOR THIS PANEL ENTRY?            
         BNH   *+6                                                              
         DC    H'0'                MUST MAKE PANEL TABLE LARGER                 
*                                                                               
         LA    R8,SDPLENQ(R8)      UPDATE A(CURRENT PANEL ENTRY)                
         MVI   0(R8),X'FF'         MARK END OF TABLE                            
*                                                                               
         B     XIT8                EXIT, BUT KEEP PANEL TABLE POINTER           
         DROP  R3                                                               
         EJECT                                                                  
* BUILD SINGLE-RECORD TWA FROM PANEL TABLE                                      
*                                                                               
BLDSTWA  NTR1                                                                   
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
*                                                                               
         LA    R3,DMCB             BUILD TWABLD PARAMETER LIST                  
         USING TWAPARMD,R3                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD                                      
         MVC   TWAPATWA,SDTWA      A(TWA)                                       
         LA    R4,TWAELEM                                                       
         USING TWAELEMD,R4                                                      
         ST    R4,TWAPAFST         A(BUILD ELEMENT)                             
*                                                                               
         BAS   RE,FNDSTROW         LOCATE NO-OP FIELD AND STARTING ROW          
         MVC   TWAPAOUT,DUB        A(FIRST OUTPUT AREA)                         
         OC    DUB+4(4),DUB+4      IS A SCROLLER SCREEN DISPLAYED NOW?          
         BZ    BST10               NO                                           
         CLI   TESTONLY,C'Y'       BUILD SCREEN ONLY?                           
         BE    BST10               YES                                          
         L     RE,DUB+4                                                         
         CLC   16(L'SDPPNLEL,RE),SDPPNLEL                                       
         BE    BSTX                THE SCREEN IS ALREADY BUILT                  
         MVC   16(L'SDPPNLEL,RE),SDPPNLEL                                       
         B     BST30                                                            
*                                                                               
BST10    XC    TWAELEM,TWAELEM     BUILD ELEMENT                                
         MVI   TWAELCD,1           ONLY ONE ELEMENT AT A TIME                   
         MVI   TWAELLN,TWAELLNQ                                                 
         MVI   TWAEATB,X'FF'       FIELD IS NO-OP                               
         MVI   TWAEFLN,L'SDPPNLEL+8  L'PANELKEY (W/OUT X'000A') + LABEL         
*                                                                               
         GOTO1 SDTWABLD,(R3)                                                    
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BST20                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BST20    L     RE,TWAPAOUT         PUT PANEL MASTER KEY IN NO-OP FIELD          
         MVC   8(8,RE),=C'*SCROLL*'  UNIQUE (HOPEFULLY) LABEL                   
         MVC   16(L'SDPPNLEL,RE),SDPPNLEL                                       
         MVC   TWAPAOUT,TWAPANXT                                                
*                                                                               
BST30    LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
         ZIC   R0,SDSTROW          STARTING ROW NUMBER                          
         SH    R0,HALF             LAST ROW WITH KEY DATA IN IT                 
         ZIC   RF,SDPROW                                                        
         AR    R0,RF               SAVE RELATIVE ROW NUMBER IN R0               
*                                                                               
BST40    CLI   0(R8),X'FF'         END OF PANEL TABLE?                          
         BE    BST110              YES                                          
*                                                                               
         CLI   SDPNAMLN,0          IS THERE A NAME FIELD TO BUILD?              
         BE    BST60               NO                                           
*                                                                               
         XC    TWAELEM,TWAELEM                                                  
         MVI   TWAELCD,1           ONLY BUILD NAME ELEMENT NOW                  
         MVI   TWAELLN,TWAELLNQ                                                 
         MVC   TWAERLN,SDPROW      ASSUME RELATIVE ROW NUMBER IS OK             
         LTR   R0,R0               FIRST DATATYPE?                              
         BZ    *+10                NO                                           
         STC   R0,TWAERLN          YES -- RELATIVE ROW WAS PUT IN R0            
         SR    R0,R0               NO LONGER THE FIRST TIME THROUGH             
         MVC   TWAECOL,SDPCOL      COLUMN NUMBER                                
         MVC   TWAEFLN,SDPNAMLN    NAME FIELD LENGTH                            
         OI    TWAEATB,X'20'       NAME IS ALWAYS PROTECTED                     
         TM    SDPATTR,X'08'       NAME HIGH-INTENSITY?                         
         BZ    *+8                                                              
         OI    TWAEATB,X'08'                                                    
         TM    SDPATTR,X'04'       NAME LOW-INTENSITY?                          
         BZ    *+8                                                              
         OI    TWAEATB,X'0C'                                                    
*                                                                               
         GOTO1 SDTWABLD,(R3)       ADD NAME FIELD TO TWA                        
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BST50                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BST50    L     RE,TWAPAOUT                                                      
         ZIC   R1,TWAEFLN          FIELD LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),SDPROWNM    PUT TEXT INTO TWA                            
         MVC   TWAPAOUT,TWAPANXT                                                
*                                                                               
BST60    CLI   SDPDATLN,0          IS THERE A DATA FIELD TO BUILD?              
         BE    BST100              NO                                           
*                                                                               
         XC    TWAELEM,TWAELEM                                                  
         MVI   TWAELCD,1           ONLY BUILD DATA ELEMENT NOW                  
         MVI   TWAELLN,TWAELLNQ                                                 
         CLI   SDPNAMLN,0          WAS THERE A NAME FIELD PREVIOUSLY?           
         BNE   *+16                YES - USE SAME ROW, USE SOFT COLUMN          
         MVC   TWAERLN,SDPROW      RELATIVE ROW NUMBER                          
         MVC   TWAECOL,SDPCOL      COLUMN NUMBER                                
         MVC   TWAEFLN,SDPDATLN    DATA FIELD LENGTH                            
         TM    SDPATTR,X'40'       DATA LOWER CASE?                             
         BZ    *+8                                                              
         OI    TWAEATB,X'40'                                                    
         TM    SDPATTR,X'20'       DATA PROTECTED?                              
         BZ    *+8                                                              
         OI    TWAEATB,X'20'                                                    
         TM    SDPATTR,X'01'       DATA HIGH-INTENSITY?                         
         BZ    *+8                                                              
         OI    TWAEATB,X'08'                                                    
         TM    SDPATTR,X'02'       DATA LOW-INTENSITY?                          
         BZ    *+8                                                              
         OI    TWAEATB,X'0C'                                                    
         MVC   TWAEFLD,SDPIDNUM    FIELD ID NUMBER                              
*                                                                               
         GOTO1 SDTWABLD,(R3)       ADD DATA FIELD TO TWA                        
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BST70                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BST70    L     RE,TWAPAOUT                                                      
         CLI   TESTONLY,C'Y'       SHOULD WE FILL UP DATA FIELDS?               
         BNE   BST90               NO                                           
*                                                                               
         LA    RF,8(RE)            A(DATA)                                      
         ZIC   R0,TWAEFLN          FIELD LENGTH                                 
         LA    R1,1                                                             
BST80    CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(1,RF),DUB                                                      
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,BST80                                                         
*                                                                               
BST90    S     RE,TWAPATWA                                                      
         STH   RE,SDPSDISP         DISPLACEMENT TO TWA FIELD                    
         MVC   TWAPAOUT,TWAPANXT                                                
*                                                                               
BST100   LA    R8,SDPLENQ(R8)      BUMP TO NEXT DATATYPE                        
         B     BST40                                                            
*                                                                               
BST110   L     R1,TWAPAOUT         DISPLAY PFKEY INSTRUCTIONS                   
         BAS   RE,BUILDPFS                                                      
*                                                                               
BSTX     B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
* BUILD MULTIPLE-RECORD TWA FROM PANEL TABLE                                    
*                                                                               
BLDMTWA  NTR1                                                                   
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
         MVI   BYTE,0              FLAGS TO SHOW IF WE NEED TOP/BOTTOM          
         LA    R1,2                START ON COLUMN 2                            
*                                                                               
BMT10    TM    SDPFLAGS,SDPBELOW   FIELD GOES BELOW OTHERS?                     
         BZ    *+12                                                             
         ST    R8,AFLDBELO         YES                                          
         B     BMT15                                                            
*                                                                               
         LR    R0,R1               SAVE CURRENT COLUMN NUMBER                   
         ZIC   RE,SDPNAMLN         NAME LENGTH                                  
         ZIC   RF,SDPDATLN         DATA LENGTH                                  
         CR    RE,RF                                                            
         BL    *+6                                                              
         LR    RF,RE               RF = GREATER OF NAME AND DATA WIDTHS         
         LA    RF,1(RF)                                                         
         AR    R1,RF               R1 = TOTAL WIDTH OF ROW SO FAR               
         CH    R1,=H'79'           HAVE WE REACHED THE MAXIMUM WIDTH?           
         BH    BMT20                                                            
*                                                                               
         STC   R0,SDPSOFTC         NO -- THIS FIELD WILL BE VISIBLE             
         CLI   KEY1COL,0           SAVE COLUMN OF FIRST KEY FIELD               
         BNE   *+16                                                             
         CLI   SDPSUBPN,C'K'                                                    
         BNE   *+8                                                              
         STC   R0,KEY1COL                                                       
         CLI   SDPCL1NL,0          ANY TOP NAME?                                
         BE    *+8                                                              
         OI    BYTE,X'80'          YES                                          
         CLI   SDPCL2NL,0          ANY BOTTOM NAME?                             
         BE    *+8                                                              
         OI    BYTE,X'40'          YES                                          
*                                                                               
BMT15    LA    R8,SDPLENQ(R8)      BUMP TO NEXT FIELD                           
         CLI   0(R8),X'FF'         END OF PANEL TABLE?                          
         BNE   BMT10                                                            
         B     BMT25               YES                                          
*                                                                               
BMT20    MVI   SDPSOFTC,0          THIS FIELD WILL NOT BE VISIBLE               
         LA    R8,SDPLENQ(R8)      BUMP TO NEXT FIELD                           
         CLI   0(R8),X'FF'         END OF PANEL TABLE?                          
         BNE   BMT20               NO                                           
*                                                                               
BMT25    LA    R3,DMCB                                                          
         USING TWAPARMD,R3                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD                                      
         MVC   TWAPATWA,SDTWA      A(TWA)                                       
*                                                                               
         TM    BYTE,X'80'          ANY TOP NAME FIELDS AT ALL?                  
         BZ    BMT50               NO                                           
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
         BAS   RE,FNDSTROW         FIND WHERE TO START BUILDING FIELDS          
         MVC   TWAPAOUT,DUB        A(FIRST OUTPUT AREA)                         
*                                                                               
         LA    R4,TWAELEM          A(TWAELEMD)                                  
         ST    R4,TWAPAFST                                                      
         USING TWAELEMD,R4                                                      
         XC    TWAELEM,TWAELEM                                                  
         MVI   TWAELCD,1           ONLY BUILD TOP NAME ELEMENT NOW              
         MVI   TWAELLN,TWAELLNQ                                                 
         MVC   TWAERLN,SDSTROW     STARTING ROW NUMBER                          
         OI    TWAERLN,X'80'       NUMBER IS ABSOLUTE                           
*                                                                               
BMT30    CLI   SDPSOFTC,0          IS THIS FIELD VISIBLE NOW?                   
         BE    BMT40               NO                                           
         CLI   SDPCL1NL,0          IS THERE A TOP NAME FIELD TO BUILD?          
         BE    BMT40               NO                                           
         TM    SDPFLAGS,SDPBELOW   FIELD GOES BELOW OTHERS?                     
         BO    BMT40               YES -- NO HEADINGS                           
*                                                                               
         MVC   TWAECOL,SDPSOFTC    SOFT COLUMN NUMBER                           
         ZIC   RE,SDPNAMLN         NAME LENGTH                                  
         ZIC   RF,SDPDATLN         DATA LENGTH                                  
         CR    RE,RF                                                            
         BL    *+6                                                              
         LR    RF,RE               RF = GREATER OF NAME AND DATA WIDTHS         
         STC   RF,TWAEFLN                                                       
         MVI   TWAEATB,X'20'       NAME IS ALWAYS PROTECTED                     
         TM    SDPATTR,X'08'       NAME HIGH-INTENSITY?                         
         BZ    *+8                                                              
         OI    TWAEATB,X'08'                                                    
         TM    SDPATTR,X'04'       NAME LOW-INTENSITY?                          
         BZ    *+8                                                              
         OI    TWAEATB,X'0C'                                                    
*                                                                               
         GOTO1 SDTWABLD,(R3)       ADD NAME FIELD TO TWA                        
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BMT35                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BMT35    L     RE,TWAPAOUT                                                      
         ZIC   R1,SDPCL1NL         TOP NAME LENGTH                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),SDPCL1NM    PUT TOP TEXT INTO TWA                        
         MVC   TWAPAOUT,TWAPANXT                                                
         MVI   TWAERLN,0           STAY ON SAME LINE FROM NOW ON                
*                                                                               
BMT40    LA    R8,SDPLENQ(R8)      BUMP TO NEXT DATATYPE                        
         CLI   0(R8),X'FF'         END OF PANEL TABLE?                          
         BNE   BMT30               NO                                           
*                                                                               
BMT50    TM    BYTE,X'40'          ANY BOTTOM NAME FIELDS AT ALL?               
         BZ    BMT80               NO                                           
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
*                                                                               
         XC    TWAELEM,TWAELEM                                                  
         MVI   TWAELCD,1           ONLY BUILD BOTTOM NAME ELEMENT NOW           
         MVI   TWAELLN,TWAELLNQ                                                 
         MVI   TWAERLN,1           GO DOWN ONE LINE                             
*                                                                               
BMT60    CLI   SDPSOFTC,0          IS THIS FIELD VISIBLE NOW?                   
         BE    BMT70               NO                                           
         CLI   SDPCL2NL,0          IS THERE A BOTTOM NAME FIELD?                
         BE    BMT70               NO                                           
         TM    SDPFLAGS,SDPBELOW   FIELD GOES BELOW OTHERS?                     
         BO    BMT70               YES -- NO HEADINGS                           
*                                                                               
         MVC   TWAECOL,SDPSOFTC    SOFT COLUMN NUMBER                           
         ZIC   RE,SDPNAMLN         NAME LENGTH                                  
         ZIC   RF,SDPDATLN         DATA LENGTH                                  
         CR    RE,RF                                                            
         BL    *+6                                                              
         LR    RF,RE               RF = GREATER OF NAME AND DATA WIDTHS         
         STC   RF,TWAEFLN                                                       
         MVI   TWAEATB,X'20'       NAME IS ALWAYS PROTECTED                     
         TM    SDPATTR,X'08'       NAME HIGH-INTENSITY?                         
         BZ    *+8                                                              
         OI    TWAEATB,X'08'                                                    
         TM    SDPATTR,X'04'       NAME LOW-INTENSITY?                          
         BZ    *+8                                                              
         OI    TWAEATB,X'0C'                                                    
*                                                                               
         GOTO1 SDTWABLD,(R3)       ADD NAME FIELD TO TWA                        
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BMT65                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BMT65    L     RE,TWAPAOUT                                                      
         ZIC   R1,SDPCL2NL         BOTTOM NAME LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),SDPCL2NM    PUT BOTTOM TEXT INTO TWA                     
         MVC   TWAPAOUT,TWAPANXT                                                
         MVI   TWAERLN,0           STAY ON SAME LINE FROM NOW ON                
*                                                                               
BMT70    LA    R8,SDPLENQ(R8)      BUMP TO NEXT DATATYPE                        
         CLI   0(R8),X'FF'         END OF PANEL TABLE?                          
         BNE   BMT60               NO                                           
*                                                                               
BMT80    LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
         XC    TWAELEM,TWAELEM                                                  
         MVI   TWAELCD,1           BUILD ONE DATA FIELD                         
         MVI   TWAELLN,TWAELLNQ                                                 
         MVI   TWAERLN,1           GO DOWN ONE LINE                             
*                                                                               
BMT90    CLI   SDPSOFTC,0          IS THIS FIELD VISIBLE NOW?                   
         BE    BMT100              NO                                           
         CLI   SDPDATLN,0          IS THERE A DATA FIELD TO BUILD?              
         BE    BMT100              NO                                           
         TM    SDPFLAGS,SDPBELOW   FIELD GOES BELOW OTHERS?                     
         BO    BMT100              YES -- DON'T BUILD YET                       
*                                                                               
         MVC   TWAECOL,SDPSOFTC    SOFT COLUMN NUMBER                           
         MVC   TWAEFLN,SDPDATLN    DATA FIELD LENGTH                            
         MVI   TWAEATB,0                                                        
         TM    SDPATTR,X'40'       DATA LOWER CASE?                             
         BZ    *+8                                                              
         OI    TWAEATB,X'40'                                                    
         TM    SDPATTR,X'20'       DATA PROTECTED?                              
         BZ    *+8                                                              
         OI    TWAEATB,X'20'                                                    
         TM    SDPATTR,X'01'       DATA HIGH-INTENSITY?                         
         BZ    *+8                                                              
         OI    TWAEATB,X'08'                                                    
         TM    SDPATTR,X'02'       DATA LOW-INTENSITY?                          
         BZ    *+8                                                              
         OI    TWAEATB,X'0C'                                                    
         MVC   TWAEFLD,SDPIDNUM    FIELD ID NUMBER                              
*                                                                               
         GOTO1 SDTWABLD,(R3)       ADD DATA FIELD TO TWA                        
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BMT91                                                            
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BMT91    TM    BYTE,X'01'          IS THIS THE FIRST LINE?                      
         BO    BMT94               NO                                           
         L     RF,TWAPAOUT                                                      
         S     RF,TWAPATWA                                                      
         STH   RF,SDPSDISP         DISPLACEMENT TO TWA FIELD                    
*&&UK                                                                           
         L     RF,SDDTYPES         A(DATATYPE TABLE)                            
         LTR   R0,RF                                                            
         BZ    BMT94               NO DATATYPE TABLE GIVEN                      
         AH    R0,SDDTASIZ         A(END OF DATATYPE TABLE)                     
         USING SDDTYPED,RF                                                      
BMT93    CLC   SDPDKEY,SDDDKEY     FIND MATCH ON DATATYPE/REPETITION ID         
         BNE   *+14                                                             
         MVC   SDDFLDHD,SDPSDISP                                                
         B     *+18                                                             
         LA    RF,SDDDTPLQ(RF)                                                  
         CR    RF,R0               END OF TABLE?                                
         BL    BMT93               NO                                           
         MVI   SDRETURN,SDRETNOD   NO MATCHING ENTRY IN DATATYPE TABLE          
         B     ERREX               TAKE ERROR EXIT                              
         DROP  RF                                                               
*&&                                                                             
BMT94    L     RF,TWAPAOUT                                                      
         ZIC   R0,TWAEFLN          FIELD LENGTH                                 
         LA    R1,1                                                             
         LA    RE,8(RF)                                                         
BMT95    CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(1,RE),DUB                                                      
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BMT95                                                         
*                                                                               
         MVC   TWAPAOUT,TWAPANXT                                                
         MVI   TWAERLN,0           STAY ON SAME LINE FROM NOW ON                
*                                                                               
BMT100   LA    R8,SDPLENQ(R8)      BUMP TO NEXT DATATYPE                        
         CLI   0(R8),X'FF'         END OF PANEL TABLE?                          
         BNE   BMT90               NO                                           
*                                                                               
         OC    AFLDBELO,AFLDBELO   ANY FIELD TO BUILD BELOW THIS ONE?           
         BZ    BMT120                                                           
         L     R8,AFLDBELO                                                      
         MVI   TWAERLN,1           GO DOWN ONE LINE                             
         ZIC   RF,KEY1COL          FIRST KEY FIELD COLUMN                       
         LA    RF,1(RF)                                                         
         STC   RF,TWAECOL          FIELD GOES JUST TO RIGHT OF KEY              
         MVC   TWAEFLN,SDPDATLN    DATA FIELD LENGTH                            
         MVI   TWAEATB,X'20'       PROTECTED                                    
         TM    SDPATTR,X'01'       DATA HIGH-INTENSITY?                         
         BZ    *+8                                                              
         OI    TWAEATB,X'08'                                                    
         TM    SDPATTR,X'02'       DATA LOW-INTENSITY?                          
         BZ    *+8                                                              
         OI    TWAEATB,X'0C'                                                    
         MVC   TWAEFLD,SDPIDNUM    FIELD ID NUMBER                              
*                                                                               
         GOTO1 SDTWABLD,(R3)       ADD DATA FIELD TO TWA                        
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    BMT110                                                           
         TM    TWAPERRS,TWAPEEMC+TWAPEEMS+TWAPEEML                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SDRETURN,SDRETTWA   INVALID TWA DESCRIPTION                      
         B     ERREX               TAKE ERROR EXIT                              
*                                                                               
BMT110   L     RF,TWAPAOUT                                                      
         MVC   TWAPAOUT,TWAPANXT                                                
         ZIC   R0,TWAEFLN          FIELD LENGTH                                 
         LA    R1,1                                                             
         LA    RE,8(RF)                                                         
BMT115   CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(1,RE),DUB                                                      
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BMT115                                                        
*                                                                               
BMT120   OI    BYTE,X'01'          NO LONGER BUILDING FIRST LIST LINE           
         SR    RE,RE                                                            
         LH    RF,2(RF)            SCREEN ADDRESS                               
         D     RE,=F'80'           80 COLUMNS PER LINE                          
         LA    RF,2(RF)            ABSOLUTE ROW NUMBER OF NEXT LINE             
         OC    AFLDBELO,AFLDBELO   DOES IT TAKE TWO LINES PER RECORD?           
         BZ    *+8                                                              
         LA    RF,1(RF)            YES                                          
         LA    RE,PANELTAB                                                      
         ZIC   R0,SDPFSTPF-SDPANELD(RE)   FIRST PFKEY LINE NUMBER               
         CR    RF,R0                                                            
         BL    BMT80                                                            
*                                                                               
         L     R1,TWAPAOUT         DISPLAY PFKEY INSTRUCTIONS                   
         BAS   RE,BUILDPFS                                                      
*                                                                               
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
* FIND THE LAST TWA ROW NUMBER WITH KEY DATA (TOP PORTION OF SCREEN),           
* AND PUT THIS NUMBER INTO 'HALF'.  ALSO, FIND THE TWA ADDRESS TO               
* START BUILDING NEW FIELDS, AND SAVE THIS ADDRESS IN 'DUB'.  IF A              
* NO-OP FIELD PREVIOUSLY CREATED BY SCROLLER IS FOUND, SAVE ITS ADDRESS         
* IN DUB+4.                                                                     
*                                                                               
FNDSTROW XC    DUB,DUB                                                          
         L     R2,SDTWA            A(TWA)                                       
         LA    R2,64(R2)           SKIP 64-BYTE HEADER                          
*                                                                               
FST10    CLI   0(R2),0             END OF TWA?                                  
         BE    FST30                                                            
         CLI   1(R2),X'FF'         NO-OP FIELD?                                 
         BNE   *+22                NO                                           
         CLC   =C'*SCROLL*',8(R2)  IS IT MY PANEL KEY FIELD?                    
         BNE   FST20               NO -- SKIP IT                                
         ST    R2,DUB+4            YES -- SAVE ITS ADDRESS                      
         B     FST20                                                            
*                                                                               
         SR    R0,R0                                                            
         LH    R1,2(R2)            SCREEN ADDRESS                               
         D     R0,=F'80'           80 COLUMNS PER LINE                          
         LA    R1,1(R1)            ABSOLUTE ROW NUMBER                          
         CLM   R1,1,SDSTROW        IS ROW >= FIRST ROW?                         
         BNL   FST30               YES                                          
         STH   R1,HALF             SAVE ROW NUMBER                              
*                                                                               
FST20    ZIC   R0,0(R2)            FIELD LENGTH                                 
         AR    R2,R0               A(NEXT FIELD HEADER)                         
         B     FST10                                                            
*                                                                               
FST30    ST    R2,DUB              SAVE A(FIRST OUTPUT AREA)                    
         BR    RE                                                               
         EJECT                                                                  
* INITIALIZE TSAR BLOCK                                                         
*                                                                               
TSARINIT NTR1                                                                   
*                                                                               
         MVI   TSACTN,TSAINI       INITIALIZE                                   
         MVC   TSPAGL,SDTWAPGL     LOW TEMPSTR PAGE NUMBER                      
         MVC   TSPAGN,SDTWAPGN     NUMBER OF TEMPSTR PAGES TO USE               
*                                                                               
         OC    SDTSARSV,SDTSARSV   ARE WE STARTING FROM SCRATCH?                
         BZ    TSI10                                                            
         MVI   TSACTN,TSARES       NO -- RESTORE PREVIOUS TSAR AREAS            
         MVC   TSINDS,SDTSINDS                                                  
         OI    TSINDS,TSIREUSE                                                  
         MVC   TSPAGL,SDTSPAGL                                                  
         MVC   TSPAGN,SDTSPAGN                                                  
*                                                                               
TSI10    MVC   TSABUF+1(3),SDTIA+1 A(14K CORE BUFFER)                           
         MVI   TSNBUF,1            NUMBER OF CORE BUFFERS                       
         MVC   TSACOM,SDCOMFCS     A(COMFACS)                                   
         OI    TSRECI,TSRVAR       RECORDS ARE VARIABLE LENGTH                  
         MVI   TSKEYL,4            KEY LENGTH                                   
         MVC   TSRECL,=H'512'      THIS IS SOMEWHAT ARBITRARY                   
         OI    TSINDS,TSIXTTWA     USE 14K TWAS                                 
         GOTO1 TSAR,(R7)                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SDTSPAGL,TSPAGL     THIS MUST BE SAVED/RESTORED BY USER          
         MVC   SDTSPAGN,TSPAGN                                                  
         MVC   SDTSINDS,TSINDS                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
READPAN  NTR1                                                                   
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING TSRECORD,R3                                                      
         MVI   TSPANNUM,C'1'                                                    
         MVI   TSRECTYP,C'P'       PANEL TABLE ENTRY RECORDS                    
         ST    R3,TSAREC           A(RECORD)                                    
         MVI   TSACTN,TSARDH                                                    
*                                                                               
RDP10    GOTO1 TSAR,(R7)           LOOK FOR NEXT PANEL ENTRY                    
         TM    TSERRS,TSEEOF                                                    
         BO    RDPX                NO MORE                                      
         MVC   0(SDPLENQ,R8),TSDATA                                             
         LA    R8,SDPLENQ(R8)                                                   
         MVI   0(R8),X'FF'         MARK END OF TABLE                            
         MVI   TSACTN,TSANXT                                                    
         B     RDP10                                                            
*                                                                               
RDPX     B     XIT                                                              
         SPACE 5                                                                
WRITEPAN NTR1                                                                   
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R3,WORK                                                          
         MVC   TSRECLEN,=Y(SDPLENQ+6)  2 FOR LENGTH, 4 FOR KEY                  
         MVI   TSPANNUM,C'1'                                                    
         MVI   TSRECTYP,C'P'       PANEL TABLE ENTRY RECORDS                    
         ST    R3,TSAREC           A(RECORD)                                    
         MVI   TSACTN,TSAADD                                                    
         LA    R2,1                                                             
*                                                                               
WRP10    STH   R2,TSSEQNUM         SEQUENCE NUMBER                              
         MVC   TSDATA(SDPLENQ),0(R8)                                            
         GOTO1 TSAR,(R7)                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,1(R2)                                                         
         LA    R8,SDPLENQ(R8)      BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R8),X'FF'         END OF TABLE?                                
         BNE   WRP10               NO                                           
*                                                                               
         MVI   TSACTN,TSASAV       WRITE TABLE TO TEMPSTR                       
         GOTO1 TSAR,(R7)                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
FILLDTYP NTR1                                                                   
*                                                                               
* PUT RELOCATABLE ADDRESSES IN THE USER'S DATATYPE TABLE                        
* (E.G., SCREEN ADDRESS, AND DATATABLE ADDRESS)                                 
*                                                                               
         OC    SDDTYPES,SDDTYPES                                                
         BZ    FDX                 NO DATATYPE TABLE GIVEN                      
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
*                                                                               
FD10     CLI   SDPDATLN,0          ANY DATA IN THIS FIELD?                      
         BE    FD40                NO                                           
         L     RF,SDDTYPES         A(DATATYPE TABLE)                            
         LR    R0,RF                                                            
         AH    R0,SDDTASIZ         A(END OF DATATYPE TABLE)                     
         USING SDDTYPED,RF                                                      
*                                                                               
FD20     CLC   SDPDKEY,SDDDKEY     FIND MATCH ON DATATYPE/REPETITION ID         
         BNE   FD30                                                             
         MVC   SDDDATLN,SDPDATLN                                                
         MVC   SDDTABLN,SDPTABLN                                                
         L     RE,SDTWA                                                         
         AH    RE,SDPSDISP         RE = A(TWA FIELD)                            
         ST    RE,SDDFLDHD         A(TWA FIELD)                                 
         L     RE,SDDTATAB                                                      
         AH    RE,SDPDDISP         RE = A(TWA FIELD)                            
         ST    RE,SDDDATA          A(DATA WITHIN DATA TABLE)                    
         B     FD40                                                             
*                                                                               
FD30     LA    RF,SDDDTPLQ(RF)                                                  
         CR    RF,R0               END OF TABLE?                                
         BL    FD20                NO                                           
         MVI   SDRETURN,SDRETNOD   NO MATCHING ENTRY IN DATATYPE TABLE          
         B     ERREX               TAKE ERROR EXIT                              
         DROP  RF                                                               
*                                                                               
FD40     LA    R8,SDPLENQ(R8)      BUMP TO NEXT PANEL TABLE ENTRY               
         CLI   0(R8),X'FF'                                                      
         BNE   FD10                                                             
*                                                                               
FDX      B     XIT                                                              
         EJECT                                                                  
* UPDATE A RECORD                                                               
*                                                                               
UPDATE   NTR1                                                                   
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
*                                                                               
UPD10    CLI   SDPDATLN,0          ANY DATA FIELD?                              
         BE    UPD20               NO                                           
*                                                                               
         ZIC   R3,SDPDTNUM         INDEX INTO DATATYPE TABLE                    
         BCTR  R3,0                                                             
         MH    R3,=Y(SDDDTPLQ)                                                  
         A     R3,SDDTYPES         A(DATATYPE TABLE ENTRY)                      
         USING SDDTYPED,R3                                                      
         ST    R3,SDTBLENT                                                      
         L     RF,SDDFLDHD                                                      
         TM    1(RF),X'20'         PROTECTED FIELD?                             
         BO    UPD20               YES -- DON'T VALIDATE IT                     
         TM    4(RF),X'20'         PREVIOUSLY VALIDATED BIT ON?                 
         BZ    *+12                NO -- VALIDATE THE FIELD                     
         TM    SDDFLAG,SDDCHNGQ    USER WANTS THIS FIELD VALIDATED?             
         BZ    UPD20               NO                                           
*                                                                               
         OC    SDDIRTN,SDDIRTN     ANY INPUT ROUTINE?                           
         BZ    UPD20               NO                                           
         XC    SDUDATA,SDUDATA     CLEAR USER'S OUTPUT AREA                     
         MVI   SDWHY,SDYINPQ       HOOK TO INPUT ROUTINE                        
         MVI   SDRETURN,SDRETOK                                                 
         BAS   RE,GOHOOK                                                        
         CLI   SDRETURN,SDRETOK    DID APPLICATION FIND AN ERROR?               
         BNE   ERREX               YES                                          
*                                                                               
         L     RF,SDDDATA          A(DATA WITHIN DATA TABLE)                    
         ZIC   R1,SDPTABLN         DATA TABLE FIELD LENGTH                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SDUDATA     PUT DATA IN DATA TABLE                       
         DROP  R3                                                               
*                                                                               
UPD20    LA    R8,SDPLENQ(R8)      BUMP TO NEXT PANEL TABLE ENTRY               
         CLI   0(R8),X'FF'                                                      
         BNE   UPD10                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY A RECORD                                                              
*                                                                               
DISPLAY  NTR1                                                                   
*                                                                               
         XC    LASTXMIT,LASTXMIT                                                
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
*                                                                               
DIS10    ZIC   R2,SDPDATLN         DATA FIELD LENGTH                            
         LTR   R2,R2               ANY DATA FIELD?                              
         BZ    DIS60               NO                                           
*                                                                               
         ZIC   R3,SDPDTNUM         INDEX INTO DATATYPE TABLE                    
         BCTR  R3,0                                                             
         MH    R3,=Y(SDDDTPLQ)                                                  
         A     R3,SDDTYPES         A(DATATYPE TABLE ENTRY)                      
         USING SDDTYPED,R3                                                      
         ST    R3,SDTBLENT                                                      
         L     RF,SDDFLDHD                                                      
         OI    4(RF),X'20'         SET PREVIOUSLY VALIDATED                     
*                                                                               
         OC    SDDXRTN,SDDXRTN     ANY TRANSFER ROUTINE?                        
         BZ    DIS20               NO                                           
*                                                                               
         XC    SDUDATA,SDUDATA     CLEAR USER'S OUTPUT AREA                     
         MVI   SDWHY,SDYXFRQ       HOOK TO TRANSFER ROUTINE                     
         BAS   RE,GOHOOK                                                        
         L     RF,SDDDATA          A(DATA WITHIN DATA TABLE)                    
         ZIC   R1,SDPTABLN         DATA TABLE FIELD LENGTH                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SDUDATA     PUT DATA IN DATA TABLE                       
*                                                                               
DIS20    XC    SDUDATA,SDUDATA     CLEAR USER'S OUTPUT AREA                     
         CLC   SDDORTN,=X'FF01'    SPECIAL KEYWORD ROUTINE?                     
         BE    *+16                NO                                           
         MVI   SDWHY,SDYOUTQ       HOOK TO OUTPUT ROUTINE                       
         BAS   RE,GOHOOK                                                        
         B     DIS30                                                            
*                                                                               
         LR    R1,R2               SCREEN FIELD LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SDUDATA(0),0(RF)    JUST PRETEND USER MOVED EBCDIC DATA          
*                                                                               
DIS30    L     RF,SDDFLDHD         A(TWA FIELD HEADER)                          
         OC    LASTXMIT,LASTXMIT   ANY FIELDS TRANSMITTED YET?                  
         BZ    DIS40               NO                                           
*                                                                               
         LH    R0,2(RF)            SCREEN ADDRESS OF THIS FIELD                 
         SH    R0,LASTXMIT         DISP FROM END OF PREVIOUSLY XMITTED          
         CH    R0,=H'2'            IF THERE'S ONLY ONE SPACE. . .               
         BE    DIS50               . . . THEN FIELD MUST BE TRANSMITTED         
*                                                                               
DIS40    LA    R0,SDUDATA                                                       
         LA    RE,SDUDATA+(L'SDUDATA-1)  RE = A(LAST BYTE OF SDUDATA)           
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+12                GOT IT                                       
         BCTR  RE,0                BACK UP                                      
         CR    RE,R0               HAVE WE GONE PAST THE BEGINNING?             
         BNL   *-12                NO                                           
         SR    RE,R0                                                            
         LA    RE,1(RE)            RE = LENGTH OF DATA                          
*                                                                               
         LA    R0,8(RF)                                                         
         LA    R1,8(R2,RF)                                                      
         BCTR  R1,0                R1 = A(LAST BYTE OF FIELD)                   
         CLI   0(R1),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+12                GOT IT                                       
         BCTR  R1,0                BACK UP                                      
         CR    R1,R0               HAVE WE GONE PAST THE BEGINNING?             
         BNL   *-12                NO                                           
         SR    R1,R0                                                            
         LA    R1,1(R1)            R1 = LENGTH OF DATA                          
*                                                                               
         CR    R1,RE               DO DATA LENGTHS MATCH?                       
         BNE   DIS50               NO -- MUST XMIT FIELD                        
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,RF),SDUDATA     DID THE DATA ON SCREEN CHANGE?               
         BE    DIS60               NO -- DON'T BOTHER TRANSMITTING              
*                                                                               
DIS50    LR    R1,R2               OVERWRITE ENTIRE FIELD                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),SDUDATA     MOVE USER'S DATA TO TWA FIELD                
         OI    6(RF),X'80'         XMIT                                         
         LH    RF,2(RF)            SCREEN ADDRESS OF LAST XMITTED FIELD         
         AR    RF,R2               ADD LENGTH OF FIELD                          
         BCTR  RF,0                                                             
         STH   RF,LASTXMIT         A(LAST BYTE OF LAST XMITTED FIELD)           
         DROP  R3                                                               
*                                                                               
DIS60    LA    R8,SDPLENQ(R8)      BUMP TO NEXT PANEL TABLE ENTRY               
         CLI   0(R8),X'FF'                                                      
         BNE   DIS10                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LIST     NTR1                                                                   
*                                                                               
         LA    R8,PANELTAB         A(PANEL TABLE)                               
         LA    R8,4*SDPLENQ(R8)    SKIP HEADER AND PFKEYS                       
*                                                                               
LIS50    CLI   SDPDATLN,0          ANY DATA FIELD?                              
         BE    LIS80               NO                                           
*                                                                               
         ZIC   RF,SDPDTNUM         INDEX INTO DATATYPE TABLE                    
         BCTR  RF,0                                                             
         MH    RF,=Y(SDDDTPLQ)                                                  
         A     RF,SDDTYPES         A(DATATYPE TABLE ENTRY)                      
         ST    RF,SDTBLENT                                                      
*                                                                               
         MVI   SDWHY,SDYXFRQ       HOOK TO TRANSFER ROUTINE                     
         BAS   RE,GOHOOK                                                        
*                                                                               
         MVI   SDWHY,SDYOUTQ       HOOK TO OUTPUT ROUTINE                       
         BAS   RE,GOHOOK                                                        
*                                                                               
LIS80    LA    R8,SDPLENQ(R8)      BUMP TO NEXT PANEL TABLE ENTRY               
         CLI   0(R8),X'FF'                                                      
         BNE   LIS50                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* GO TO THE USER HOOK                                                           
*                                                                               
GOHOOK   NTR1                                                                   
*                                                                               
         ICM   RF,15,SDHOOK        A(APPLICATION HOOK)                          
         BZ    GOHOOKX                                                          
         L     RE,SDUSERRD         USER'S RD                                    
         LM    R0,RC,20(RE)        RESTORE USER'S R0-RC                         
         BASR  RE,RF               GO TO HOOK                                   
*                                                                               
GOHOOKX  XIT1                                                                   
         SPACE 5                                                                
ERREX    L     RD,SAVERD           BACK ALL THE WAY OUT                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         GETEL R5,42,ELCODE                                                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
DMWORK   DS    12D                                                              
DUB      DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SAVERD   DS    F                   RD UPON ENTRY TO SCROLLER                    
WORK     DS    XL256               GENERAL WORK AREA                            
CALLOV   DS    V                   A(CALLOV)                                    
DATAMGR  DS    V                   A(DATAMGR)                                   
MINIO    DS    V                   A(MINIO)                                     
TWABLD   DS    V                   A(TWABLD)                                    
TSAR     DS    V                   A(TSAR)                                      
APARAMS  DS    A                   A(PARAMS)                                    
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
AFLDBELO DS    A                   A(PANEL ENTRY FOR 'BELOW' FIELD)             
LASTXMIT DS    H                   ROW NUMBER OF LAST TRANSMITTED FIELD         
KEY      DS    XL40                GENDIR KEY                                   
PANELKEY DS    XL40                                                             
TWAELEM  DS    XL(TWAELLNQ)        BUILD TWABLD ELEMENT HERE                    
TWAELEMX DS    X                   ALWAYS NULL (END OF ELEMENT LIST)            
ELCODE   DS    X                                                                
WRTPANEL DS    X                   C'Y' IF WE SHOULD SAVE PANEL TABLE           
TESTONLY DS    C                   C'Y' IF WE ARE ONLY SHOWING SCREEN           
KEY1COL  DS    X                   COLUMN NO. OF 1ST KEY FIELD (MULT)           
         DS    0D                                                               
TSARBLK  DS    (TSARDL)X           TSAR PARAMETER BLOCK                         
         DS    0D                                                               
DTYPEIO  DS    2000X               I/O AREA FOR DATATYPE RECORDS                
         DS    0D                                                               
PANELTAB DS    14336X                                                           
PANELTLQ EQU   *-PANELTAB          LENGTH OF PANEL TABLE                        
*                                                                               
WORKDX   EQU   *                                                                
         EJECT                                                                  
SDPANELD DSECT                                                                  
*                                                                               
SDPDKEY  DS    0CL(L'SDPDTYPE+L'SDPREPID)                                       
SDPDTYPE DS    CL7                 DATATYPE CODE (BLANK-PADDED)                 
SDPREPID DS    X                   REPETITION ID CODE                           
SDPROW   DS    X                   RELATIVE ROW NUMBER                          
SDPCOL   DS    X                   COLUMN NUMBER                                
SDPATTR  DS    X                   ATTRIBUTE FLAGS                              
*                                  X'40' = DATA FIELD UPPER/LOWER               
*                                          (NAME FIELDS ALWAYS U/L)             
*                                  X'20' = DATA FIELD PROTECTED                 
*                                          (NAME FIELDS ALWAYS PROT)            
*                                  X'08' = NAME HIGH INTENSITY                  
*                                  X'04' = NAME ZERO INTENSITY                  
*                                   OTHERWISE, NORMAL INTENSITY                 
*                                  X'01' = DATA HIGH INTENSITY                  
*                                  X'02' = DATA ZERO INTENSITY                  
*                                   OTHERWISE, NORMAL INTENSITY                 
SDPNAMLN DS    X                   NAME FIELD LENGTH                            
SDPDATLN DS    X                   DATA FIELD LENGTH                            
SDPSOFTC DS    X                   SOFT COLUMN NUMBER FROM HEADINGS             
SDPDTNUM DS    X                   INDEX INTO DATATYPE TABLE (BASE = 1)         
SDPSUBPN DS    C                   SUB-PANEL ID                                 
SDPSDISP DS    H                   DISPLACEMENT TO TWA FIELD HEADER             
SDPDDISP DS    H                   DISPLACEMENT TO DATA TABLE FIELD             
*                                                                               
SDPTABLN DS    X                   DATA TABLE FIELD LENGTH                      
SDPIDNUM DS    X                   FIELD ID NUMBER (TWA EXTENDED HEADS)         
SDPFLAGS DS    X                   FLAG BITS                                    
SDPFIXED EQU   X'80'               FIXED FIELD (ELSE FLOATING)                  
SDPSUPPR EQU   X'40'               SUPPRESS FIELD IN TWA                        
SDPVISIB EQU   X'20'               FIELD IS CURRENTLY ON SCREEN                 
SDPBELOW EQU   X'10'               FIELD IS BELOW OTHERS ON MULTIPLE            
SDPROWNM DS    CL24                ROW NAME                                     
SDPROWNL DS    X                   LENGTH OF ROW NAME                           
         DS    X                   SPARE                                        
         ORG   SDPROWNM                                                         
SDPCL1NM DS    CL12                TOP COLUMN NAME                              
SDPCL1NL DS    X                   LENGTH OF TOP COLUMN NAME                    
SDPCL2NM DS    CL12                BOTTOM COLUMN NAME                           
SDPCL2NL DS    X                   LENGTH OF BOTTOM COLUMN NAME                 
         DS    XL15                SPARE                                        
SDPLENQ  EQU   *-SDPANELD                                                       
*                                                                               
* PANEL TABLE HEADER (ALWAYS AT BEGINNING OF TABLE - FOUR ENTRIES LONG)         
*                                                                               
         ORG   SDPANELD                                                         
         DS    D                   C'*PNLTAB*'                                  
SDPPNLEL DS    CL24                PANEL MASTER KEY (WITHOUT X'000A')           
SDPNUMUS DS    X                   NUMBER OF USER LINES                         
SDPFSTPF DS    X                   LINE NUMBER OF FIRST PFKEY LINE              
         DS    XL(SDPLENQ-(*-SDPANELD)) SPARE                                   
SDPFKEYS DS    24CL8               24 PF KEYS                                   
         EJECT                                                                  
TSRECORD DSECT                                                                  
TSRECLEN DS    H                   RECORD LENGTH                                
TSPANNUM DS    C                   PANEL NUMBER (C'1', C'2', ETC.)              
TSRECTYP DS    C                   C'P' = PANEL, C'V' = VALUE (DATA)            
TSSEQNUM DS    H                   SEQUENCE NUMBER                              
TSDATA   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDSCROLLD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENPNL                                                       
         EJECT                                                                  
       ++INCLUDE CTGENDTYPE                                                     
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDSCROLL  05/01/02'                                      
         END                                                                    
