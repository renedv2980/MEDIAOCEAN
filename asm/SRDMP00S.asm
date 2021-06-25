*          DATA SET SRDMP00S   AT LEVEL 111 AS OF 05/01/02                      
*PHASE T15D00A                                                                  
         TITLE '$ND (T15D00) -- NEW ON-LINE DUMP'                               
         PRINT NOGEN                                                            
NDUMP    CSECT                                                                  
         NMOD1 WRKX-WRKD,**$DMP**,R8,RR=R3                                      
         USING WRKD,RC                                                          
         LA    RE,WRKD                                                          
         LA    RF,CLRX-WRKD                                                     
         SR    R5,R5                                                            
         MVCL  RE,R4               CLEAR FIRST PART OF W/S                      
*                                                                               
         ST    RD,SAVERD                                                        
         ST    R3,RELO                                                          
         L     R9,0(R1)                                                         
         USING SYSFACD,R9          R9=A(SYSTEM FACILITIES)                      
         L     RA,20(R1)                                                        
         USING T15DFFD,RA          RA=A(TWA)                                    
*                                                                               
         L     RF,=A(EXTRACT)      EXTRACT AND SAVE INFO FROM SSB ETC           
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         XC    IND,IND                                                          
*                                                                               
         XC    HELP,HELP                                                        
         MVC   HELPKEY,HELPID                                                   
         GOTO1 =A(HELPSCAN),DMCB,(RC),(R9),(RA),RR=RELO  ? HELP ?               
         EJECT                                                                  
***********************************************************************         
* SAVE THE ATTRIBUTES OF THE DUMP FILE AND SETS IT UP FOR READING     *         
***********************************************************************         
         SPACE 1                                                                
         L     R6,VDMPFILE         A(DMPFILE) OF DATAMGR SYSTEM FILES           
         USING DTFPHD,R6           TEMPLATE FOR DIRECT ACCESS FILES             
         XC    P1(24),P1           CLEAR OUT THE PARAMETERS (P?)                
         MVC   P1,VDARPT           DIRECT ACCESS REPORT? (FILE ATTRIB)          
         MVC   P3+2(2),=AL2(RLEN)  RECORD LENGTH                                
         ST    R6,P4               A(DUMP FILE)                                 
         GOTO1 VDADDS,P1           GET FILE ATTRIBUTES                          
         LH    RF,P3+2             ANY RECORDS ON TRACK?                        
         LTR   RF,RF                                                            
         BNZ   *+6                 YES                                          
         DC    H'0'                NO                                           
         ST    RF,RECTRK           SAVE # OF RECS PER TRK                       
         L     RF,P2                                                            
         LH    RF,2(RF)                                                         
         ST    RF,TRKCYL           SAVE TRKS PER CYL                            
* SET UP VDADDS FOR READING                                                     
         MVC   P1,VRDID            INITIALISE DADDS PARAMS FOR READ             
         LA    RF,INA              A(RECORD AREA) TO 2ND PARAMETER              
         ST    RF,P2                                                            
         XC    P3,P3               NOT NEEDED, SET BY DADDS                     
         ST    R6,P4               A(DUMP FILE)                                 
         LA    RF,ADDR             DISK ADDRESS CHANGED LATER                   
         ST    RF,P5               A(DISK ADDRESS) TO 5TH PARAMETER             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE DUMP NUMBER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VNUM     NI    DMPFLGS1,255-DMPDSCTD                                            
         CLI   HLPFLD,2            HELP REQUESTED FOR NUMBER?                   
         BE    HELPOUT             YES                                          
*                                                                               
         L     R6,VSSB             VALIDATE DUMP NUMBER                         
         USING SSBD,R6                                                          
         STAR  CLEAR=Y,ARS=ON                                                   
         OC    SSBTBLET,SSBTBLET                                                
         BNZ   *+6                                                              
         DC    H'0'                WRONG VERSION OF =DUMP                       
*                                                                               
         LAM   R2,R2,SSBTBLET                                                   
         XR    R2,R2                                                            
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
         LA    R2,TDLENQ(,R2)      FIRST SLOT EMPTY                             
*                                                                               
VNUMAA   OC    TDSYSNA,TDSYSNA                                                  
         BZ    VNUMAB                                                           
         CLC   TDSYSNA,SSBSYSN4                                                 
         BE    VNUMAC                                                           
         LA    R2,TDLENQ(,R2)                                                   
         B     VNUMAA                                                           
*                                                                               
VNUMAB   XR    R2,R2               USE FIRST DUMP IF NOT FOUND                  
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         LA    R2,TDLENQ(,R2)      FIRST SLOT EMPTY                             
*                                                                               
VNUMAC   ICM   RF,15,TDDUMPNO      COPY CURRENT AND FIRST NUMBERS               
         STC   RF,DUMPNUM                                                       
*                                                                               
         XR    RF,RF                                                            
         XR    R2,R2                                                            
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         LA    R2,TDLENQ(,R2)                                                   
*                                                                               
VNUMAD   OC    TDSYSNA,TDSYSNA                                                  
         BZ    VNUMAE                                                           
         CLM   RF,15,TDDUMPMX      SET HIGHEST DUMP IN DATASPACE                
         BH    *+8                                                              
         ICM   RF,15,TDDUMPMX                                                   
         LA    R2,TDLENQ(,R2)                                                   
         B     VNUMAD                                                           
*                                                                               
VNUMAE   STC   RF,DUMPMAX          SAVE HIGHEST DUMP FOUND                      
         REAR  ARS=OFF                                                          
*                                                                               
         LA    R2,SRVP4H           POINT TO DUMP NUMBER FIELD                   
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   VNUMA                                                            
*                                                                               
         CLI   DUMPNUM,0           CHANGE DUMP #0 TO #1                         
         BNE   *+8                                                              
         MVI   DUMPNUM,1                                                        
*                                                                               
         EDIT  (B1,DUMPNUM),(3,SRVP4),ALIGN=LEFT                                
         OI    FHOI,FHOITR                                                      
         B     VNUMH               THEN DEFAULT IS LAST IN SSB                  
*                                                                               
VNUMA    GOTO1 VSCANNER,DMCB,(R2),(X'8A',BLOCK)   10 LINES                      
         CLI   DMCB+4,0                                                         
         BE    ERR3                                                             
         CLI   DMCB+4,1                                                         
         BE    *+8                                                              
         OI    DMPFLGS1,DMPDSCTD   CALL HABER'S DSECT DISPLAYER                 
*                                                                               
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
         CLI   SC1STLEN,0          ANY DUMP NUMBER?                             
         BNE   VNUMB               YES                                          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,DUMPNUM                                                     
         BNZ   *+8                                                              
         LA    R0,1(R0)                                                         
         STCM  R0,15,SC1STNUM      FOOL PROGRAM                                 
*                                                                               
         EDIT  (R0),(3,WORK),ALIGN=LEFT    SHOW CURRENT DUMP #                  
         LR    R1,R0                                                            
         STC   R0,SC1STNUM                                                      
         LA    R1,WORK(R1)                                                      
         MVC   0(L'SRVP4,R1),SRVP4                                              
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FHIL                                                          
         AR    R1,R0                                                            
         STC   R1,FHIL                                                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SRVP4(0),WORK                                                    
         OI    FHOI,FHOITR                                                      
         B     VNUMG                                                            
*                                                                               
VNUMB    CLI   SC2NDLEN,0          CAN'T HAVE  "DUMP # = ???"                   
         BNE   ERR3                                                             
         TM    SC1STVAL,SCNUMQ     IS IT VALID NUMERIC?                         
         BNZ   VNUMG               YES                                          
*                                                                               
VNUMD    CLI   SC1STFLD,C'A'                                                    
         BNE   ERR3                                                             
         MVI   IND,C'A'            'ALL' REQUEST                                
*                                                                               
         CLI   HLPFLD,0            CHECK HELP REQUESTED                         
         BNE   SETDMP                                                           
         GOTO1 =A(NALL),DMCB,(RC),(R9),(RA),RR=RELO                             
         B     DMP8                                                             
*                                                                               
VNUMG    ICM   RF,15,SC1STNUM                                                   
         BNZ   *+8                                                              
         LA    RF,1                                                             
         CH    RF,=H'255'          SCOPE 1 BYTE RANGE                           
         BNL   VNUMI                                                            
         STC   RF,DUMPNUM          SAVE THE DUMP NUMBER WE GOT                  
*                                                                               
VNUMH    CLC   DUMPNUM,DUMPMAX     TEST IF GT MAX DUMP NUMBER                   
         BNH   VNUMX               IT'S NOT GREATER                             
*                                                                               
VNUMI    MVC   DUMPNUM,DUMPMAX     COPY THE MAX DUMP NUMBER                     
         B     ERR3                ERROR                                        
         DROP  R2,R3                                                            
*                                                                               
VNUMX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* FIND WHERE IN THE DUMP FILE THE START OF THE DUMP IS                *         
***********************************************************************         
         SPACE 1                                                                
SETDMP   XR    R0,R0                                                            
         IC    R0,DUMPNUM          COPY THE DUMP NUMBER                         
         BCTR  R0,0                OFFSET OF DUMP NUMBER 1                      
         SR    R1,R1                                                            
         IC    R1,SSBDMPCY         R1=NUM OF CYLS IN DUMP                       
         MR    R0,R0               FIND WHICH CYLINDER DUMP IS IN               
         M     R0,TRKCYL           FIND WHICH TRACK DUMP IS IN                  
         LA    R1,1(R1)                  T = TRACK                              
         SLA   R1,16                     B = BLOCK                              
         ST    R1,ADDRSTR          SAVE TTTTBB00 OF START OF DUMP               
         ST    R1,ADDR             SAVE TTTTBB00 FOR READ OF DUMP               
*                                                                               
         BRAS  RE,READDSK          READ DUMP FILE HEADER                        
         BNE   DSKERR                                                           
*                                                                               
         LA    RE,DHDR                                                          
         LA    RF,DMPHDRL                                                       
         LA    R0,INA                                                           
         LR    R1,RF                                                            
         MVCL  RE,R0               COPY HEADER INTO OUR STORAGE                 
*                                                                               
         GOTO1 =A(READTWAB),DMCB,(RC),(R9),RR=RELO  READ SAVED TWA              
*                                                                               
SETDMPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR PFKEY PRESSED                                             *         
***********************************************************************         
         SPACE 1                                                                
PFKEYS   L     R1,ATIOB            LOAD A(TIOB)                                 
         USING TIOBD,R1            OVERLAY THE A(TIOB)                          
*                                                                               
         NI    SRVIDH+6,X'FF'-X'40'     UNSET CURSOR                            
         OI    SRVP1H+6,X'40'                                                   
         CLI   TIOBAID,0           IS IT AN ENTER KEY?                          
         BE    PFKEYSX             YES, PROCEED AS NORMAL                       
*                                                                               
         ZIC   R0,TIOBAID          LOAD THE PFKEY PRESSED                       
         CH    R0,=H'12'           PF13 - PF24?                                 
         BNH   *+8                 NO, PF1 - PF12                               
         SH    R0,=H'12'           PF13 -> PF1 .. PF24 -> PF12                  
*                                                                               
PFKEYS7  CLM   R0,1,=AL1(7)        PF7?                                         
         BL    PFKEYSX                                                          
PFKEYS8  CLM   R0,1,=AL1(8)        PF8?                                         
         BH    PFKEYSX                                                          
         STC   R0,IND              ONLY PFK7 & 8 SUPPORTED                      
         B     DMP8                PROCESS THEM                                 
*                                                                               
PFKEYSX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR SEARCH ARGUMENT                                           *         
***********************************************************************         
         SPACE 1                                                                
SRCH     CLI   HLPFLD,5            HELP REQUESTED FOR P4?                       
         BE    SRCHX               YES                                          
         LA    R2,SRVP3H                                                        
         USING FHD,R2                                                           
         CLI   FHIL,0              FIELD INPUT?                                 
         BE    SRCHX               NO                                           
*                                                                               
         CLC   FHDA(2),=C'C'''   * ONLY 2 VALID TYPES C'...'                    
         BNE   SRCHA                                                            
         CLI   FHIL,3              THREE CHARACTERS OR LESS?                    
         BNH   ERR4                INVALID - NO ARGUMENT                        
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FHIL             WHOLE ARGUMENT IN FIELD                      
         BCTR  R1,0                -1                                           
         LA    RF,FHDA(R1)         POINT TO LAST CHARACTER                      
         CLI   0(RF),C''''         IS THERE AND END QUOTE?                      
         BNE   ERR5                NEED AN END QUOTE                            
*                                                                               
         SH    R1,=H'2'            MINUS LENGTH OF C''                          
         MVI   IND,C'S'            SEARCH INDICATOR                             
         STC   R1,STRLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SRCHSTR(0),FHDA+2   SKIP THE C' AND COPY STRING                  
         B     SRCHX                                                            
*                                                                               
SRCHA    CLC   FHDA(2),=C'X'''   * AND X'...'                                   
         BNE   SRCHX                                                            
         CLI   FHIL,4              ONLY FOUR CHARACTERS OR LESS?                
         BNH   ERR4                ERROR, HEX NEEDS EVEN NUMBER                 
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FHIL             WHOLE ARGUMENT IN FIELD                      
         BCTR  R1,0                -1                                           
         LA    RF,FHDA(R1)         POINT TO LAST CHARACTER                      
         CLI   0(RF),C''''         IS THERE AND END QUOTE?                      
         BNE   ERR5                NEED AN END QUOTE                            
*                                                                               
         TM    FHIL,X'01'          ODD-NUMBER LENGTH, EVEN NUMBER STR           
         BZ    ERR4                ERROR, HEX NEEDS EVEN NUMBER                 
         SH    R1,=H'2'            MINUS LENGTH OF C''                          
         LR    R3,R1               MAKE A COPY OF LENGTH                        
*                                                                               
         GOTO1 VHEXIN,DMCB,FHDA+2,SRCHSTR,(R3)  CONVERT TO HEX                  
         CLI   15(R1),0            LENGTH ZERO?                                 
         BE    ERR4                                                             
*                                                                               
         MVI   IND,C'S'            SEARCH INDICATOR                             
         MVC   STRLEN,15(R1)       COPY THE LENGTH OF DESTINATION               
         DROP  R2                                                               
*                                                                               
SRCHX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK DISPLACEMENT                                                  *         
***********************************************************************         
         SPACE 1                                                                
DMP1     LA    R2,SRVP1H           R2=A(DISPLACEMENT)                           
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BE    DMP2                NO DISPLACEMENT, CHECK FOR REGISTER          
         CLI   HLPFLD,3                                                         
         BE    HELPOUT                                                          
*                                                                               
         TM    FHII,FHIIHE         TEST IF HEX                                  
         BZ    DMP5                NOT VALID HEX, CHECK FOR OPTIONS             
*                                                                               
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO  HEX->FULL WORD           
         MVC   START,FULL          MAKE A COPY TO START                         
         DROP  R2                                                               
*                                                                               
DMP1X    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK REGISTER                                                      *         
***********************************************************************         
         SPACE 1                                                                
DMP2     LA    R2,SRVP2H           R2=A(REGISTER)                               
         USING FHD,R2                                                           
         CLI   HLPFLD,4                                                         
         BE    HELPOUT                                                          
*                                                                               
DMP2A    CLI   FHIL,0              INPUT?                                       
         BNE   DMP3                YES - VALIDATE                               
         CLI   SRVP1H+(FHIL-FHD),0 DISPLACMENT INPUT?                           
         BNE   DMP8                ONLY DISPLACEMENT INPUT                      
*                                                                               
         MVI   FHDA,C'B'           NO PARAMS INPUTTED                           
         MVI   FHIL,1              DEFAULT IS BASE REGISTER                     
*                                                                               
DMP2X    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
DMP3     XR    R3,R3                                                            
         IC    R3,FHIL             R3=INPUT LENGTH                              
         LA    R4,FHDA             R4=CONTENTS OF FIELD                         
         CLI   0(R4),C'R'          ACCEPT REGISTER WITH 'R' PREFIX              
         BNE   DMP3A               DOESN'T BEGIN WITH AN 'R'                    
*                                                                               
         LA    R4,1(R4)            SKIP THE 'R'                                 
         MVI   XREGS,C'N'          NOT AN EXTENDED REGISTER                     
         BCT   R3,DMP3B            DECREMENT LENGTH BECUASE OF SKIP             
         B     ERR1                JUST AN 'R' IS AN ERROR                      
*                                                                               
DMP3A    CLI   0(R4),C'X'          ACCEPT REGISTER WITH 'X' PREFIX              
         BNE   DMP3B               DOESN'T BEGIN WITH AN 'X'                    
         LA    R4,1(R4)            SKIP THE 'X'                                 
         MVI   XREGS,C'Y'          EXTENDED REGISTER                            
         BCT   R3,DMP3B            DECREMENT LENGTH BECUASE OF SKIP             
         B     ERR1                JUST AN 'X' IS AN ERROR                      
*                                                                               
DMP3B    CH    R3,=H'1'            INPUT IS LENGTH OF 1?                        
         BH    DMP4                NO, CHECK FOR INDIRECT ADDRESSING            
*                                                                               
         MVC   REGADD,0(R4)        COPY THE CHARACTER                           
         CLI   0(R4),C'A'          MAKE SURE IT IS A HEX DIGIT                  
         BL    ERR1                GOTO    DMP8    IF IT IS                     
         CLI   0(R4),C'F'                                                       
         BNH   DMP8                                                             
         CLI   0(R4),C'0'                                                       
         BL    ERR1                                                             
         CLI   0(R4),C'9'                                                       
         BH    ERR1                                                             
         B     DMP8                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK FOR INDIRECT ADDRESSING                                       *         
***********************************************************************         
         SPACE 1                                                                
DMP4     CLI   IND,C'S'            SEARCH MODE AND INDIRECT ADDRESSING          
         BE    DMP4A                                                            
         CLI   IND,0               INDIRECT OR TWA AND 3RD FIELD?               
         BNE   ERR1                YES, INVALID THEN                            
*                                                                               
DMP4A    CLI   SRVP1H+(FHIL-FHD),0 DISPLACEMENT ENTERED?                        
         BE    ERR1                NEED DISP FOR INDIRECT ALSO                  
*                                                                               
* MUST HAVE ENTERED P1=ADDRESS AS P2 IS NOT A REGISTER                          
*                                                                               
DMP4B    TM    FHII,FHIIHE         IS P2 VALID HEX ?                            
         BNZ   DMP4C               YES, GO CONVERT IT                           
         CLI   FHDA,C'-'           NEGATIVE VALUE?                              
         BNE   ERR1                NO, NOT VALID HEX AND NOT NEGATIVE           
         MVI   HALF,C'-'           INDICATE NEGATIVE VALUE                      
         MVI   FHDA,C'0'           REMOVE MINUS SIGN                            
*                                                                               
DMP4C    GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO  HEX->FULL WORD           
         NC    DMCB+12(4),DMCB+12  WAS IT VALID HEX ?                           
         BZ    ERR1                NO                                           
         MVC   DISP,FULL           SAVE AS DISP FROM START                      
         CLI   HALF,0              WAS IT A NEGATIVE VALUE?                     
         BE    DMP8                NO.                                          
         L     R1,DISP                                                          
         LCR   R1,R1               GET NEGATIVE VALUE (COMPLEMENT)              
         ST    R1,DISP             ADJUST THE DISPLACEMENT                      
         B     DMP8                                                             
         EJECT                                                                  
***********************************************************************         
* CAME FROM  DMP1  (1ST FIELD NOT A VALID HEX)                        *         
***********************************************************************         
         SPACE 1                                                                
DMP5     XR    R1,R1                                                            
         IC    R1,FHIL             FIELD LENGTH                                 
         BCTR  R1,0                                                             
         LA    RF,D5IND            INDIRECT ADDRESSING?                         
         EX    R1,DMP5CMP                                                       
         BE    DMP5A                                                            
         LA    RF,D5TWA            TWA REFERENCE?                               
         EX    R1,DMP5CMP                                                       
         BE    DMP5A                                                            
         LA    RF,D5TTU            TEXT OF UPPER TWA?                           
         EX    R1,DMP5CMP                                                       
         BNE   *+12                                                             
         MVI   IND,C'U'            VIEW THE TWA (UPPER PORTION)                 
         B     DMP5B                                                            
*                                                                               
         LA    RF,D5TTL            TEXT OF LOWER TWA?                           
         EX    R1,DMP5CMP                                                       
         BNE   DMP5X                                                            
         MVI   IND,C'V'            VIEW THE TWA (LOWER PORTION)                 
         B     DMP5B                                                            
*                                                                               
DMP5A    MVC   IND,FHDA            1ST LETTER OF 'INDIRECT' OR 'TWA'            
*                                                                               
DMP5B    LA    R2,SRVP2H                                                        
         CLI   FHIL,0              NOTHING IN 2ND FIELD?                        
         BE    DMP5D               NOTHING, NO DISPLACEMENT                     
         TM    FHII,FHIIHE         VALID HEX?                                   
         BZ    DMP5C               NO                                           
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO  HEX->FULL WORD           
         MVC   START,FULL          START := DISPLACEMENT                        
         B     DMP5D                                                            
*                                                                               
DMP5C    CLI   IND,C'I'            INDIRECT AND NOT VALID HEX?                  
         BE    DMP7A               NON-HEX I/P MAY BE KEYWORD FOR 'I'           
         XR    R1,R1                                                            
         IC    R1,FHIL             LENGTH OF 2ND FIELD                          
         BCTR  R1,0                -1 FOR CLC                                   
         LA    RF,D5SCN                                                         
         EX    R1,DMP5CMP                                                       
         BNE   ERR1                INVALID I/P                                  
         MVI   TMODE,C'S'          MARK SCAN TWA MODE                           
*                                                                               
DMP5D    CLI   IND,C'I'            NOTHING TO DO WITH TWA?                      
         BE    DMP5E               NOT TWA                                      
         CLC   START,=F'64'                                                     
         BNL   DMP5E                                                            
         MVC   START,=F'64'        SKIP THE 64 BYTE HEADER OF TWA               
*                                                                               
DMP5E    LA    R2,SRVP3H           CHECK CONTENTS OF 3RD FIELD                  
         B     DMP2A                   (REGISTER)                               
*                                                                               
DMP5CMP  CLC   FHDA(0),0(RF)                                                    
*                                                                               
D5IND    DC    CL8'INDIRECT'                                                    
D5TWA    DC    CL8'TWA'                                                         
D5TTU    DC    CL8'TTU'                                                         
D5TTL    DC    CL8'TTL'                                                         
D5SCN    DC    CL8'SCAN'                                                        
*                                                                               
DMP5X    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CAME FROM  DMP5  (NOT INDIRECT OR TWA). R1=LENGTH OF 1ST FIELD      *         
* RE: D-CHAIN                                                         *         
***********************************************************************         
         SPACE 1                                                                
DMP6     LA    RF,D6X                                                           
         EX    R1,DMP6CMP          D-CHAIN OPTION?                              
         BE    DMP6A               YES                                          
         LA    RF,D6SRX                                                         
         EX    R1,DMP6CMP          SET REGISTER OPTION                          
         BNE   DMP6X               NOT SRX SO MAY BE LIST                       
*                                                                               
         OI    STATFLAG,SETREGS    INDICATE SET REGISTER OPTION                 
         OI    SAVEDFLG,X'80'      HOLD VALUE OF REGISTERS                      
         MVI   SRXCLEAR,0          CLEAR NOT SET BY USER                        
*                                                                               
         CLI   SRVP2H+(FHIL-FHD),1 LENGTH OF 1?                                 
         BNE   DMP6A               NO                                           
         CLI   SRVP2,C'C'          SET REGS TO WHAT THEY ARE @ DUMP             
         BNE   DMP6A               NO                                           
*                                                                               
         NI    STATFLAG,X'FF'-SETREGS INDICATE NOT SET REGISTER OPTION          
         NI    SAVEDFLG,X'FF'-X'80'   READ REGISTERS FROM DUMP FILE             
         MVI   SRXCLEAR,1          CLEAR SET BY USER                            
         XC    SRVP1,SRVP1                                                      
         XC    SRVP2,SRVP2                                                      
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP1H+(FHIL-FHD),0                                              
         MVI   SRVP2H+(FHIL-FHD),0                                              
         MVI   SRVP3H+(FHIL-FHD),0                                              
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    START,START                                                      
         XC    DISP,DISP                                                        
         B     DMP1                                                             
*                                                                               
DMP6A    LA    R2,SRVP2H           CHECK FOR # TO GO BACK ON D-CHAIN            
         LA    R1,1                DEFAULT IS ONE                               
         MVI   IND,C'X'            D-CHAIN BACKWARD POINTERS                    
         CLI   FHIL,0              NOTHING?                                     
         BE    DMP6D               STORE # TO GO BACK                           
*                                                                               
         TM    FHII,FHIINU         VALID NUMERIC?                               
         BNZ   DMP6C               YES                                          
         CLI   FHDA,C'-'           FORWARD POINTERS?                            
         BNE   ERR1                NO, NOTHING ELSE ALLOWED                     
         CLI   FHIL,1              JUST A MINUS SIGN?                           
         BE    ERR1                YES                                          
*                                                                               
         LA    R1,FHDA+1           POINT TO THE DATA NOT MINUS SIGN             
         XR    R3,R3                                                            
         IC    R3,FHIL             LOAD LENGTH                                  
         BCTR  R3,0                NOT THE MINUS SIGN                           
*                                                                               
DMP6B    CLI   0(R1),C'0'          MAKE SURE VALID NUMERIC                      
         BL    ERR1                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERR1                                                             
         LA    R1,1(R1)            CHECK NEXT CHARACTER                         
         BCT   R3,DMP6B            UNTIL NO MORE CHARACTERS                     
*                                                                               
         IC    R3,FHIL             INPUT LENGTH                                 
         BCTR  R3,R0               -1 BECAUSE THE MINUS SIGN                    
         BCTR  R3,R0               -1 FOR PACK                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FHDA+1          LOAD NUMERIC                                 
         CVB   R1,DUB                AND CONVERT TO BINARY                      
         MVI   IND,C'Y'            D-CHAIN FORWARD POINTERS                     
         BCTR  R1,0                FORWARD POINTER, EXACTLY ON RD               
         B     DMP6D                                                            
*                                                                               
DMP6C    IC    R3,5(R2)            INPUT LENGTH                                 
         BCTR  R3,R0               -1 FOR PACK                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FHDA            LOAD NUMERIC                                 
         CVB   R1,DUB                AND CONVERT TO BINARY                      
         LA    R1,1(R1)            1ST ONE ON D-CHAIN NOT CURRENT               
*                                                                               
DMP6D    STC   R1,NX               STORE # TO GO BACK ON D-CHAIN                
         MVI   REGADD,C'D'         D-CHAIN ADDRESS IN RD                        
         B     DMP8                                                             
*                                                                               
DMP6CMP  CLC   FHDA(0),0(RF)                                                    
*                                                                               
D6X      DC    CL8'X'                                                           
D6SRX    DC    CL8'SRX'                                                         
*                                                                               
DMP6X    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CAME FROM  DMP6  (NOT INDIRECT, TWA, OR D-CHAIN)                    *         
***********************************************************************         
         SPACE 1                                                                
LISTF    EX    R1,*+8                                                           
         BNE   LISTFX              NOT LIST MAY BE GOTO ENTRY IN LIST           
         CLC   FHDA(0),LFLIST      SEE IF LIST OPTION                           
*                                                                               
         CLC   LFLSTL,SRVID+1      AUTO LISTING?                                
         BNE   LISTFA              NO REGULAR LISTING                           
         MVI   IND,C'M'            LETTER CLOSEST TO L                          
         B     DMP8                                                             
*                                                                               
LISTFA   MVI   IND,C'L'            INDICATE LIST MODE                           
         B     DMP8                GET OTHER DUMP INFORMATION                   
*                                                                               
LFLIST   DC    CL8'LIST'                                                        
LFLSTL   DC    C'ND,L'                                                          
*                                                                               
LISTFX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CAME FROM  LISTF (NOT INDIRECT, TWA, D-CHAIN, OR LIST)              *         
***********************************************************************         
         SPACE 1                                                                
GOINLIST EX    R1,*+8              GOTO ENTRY IN LIST OPTION?                   
         BNE   GOINLX              NO                                           
         CLC   FHDA(0),GIGOTO                                                   
         MVI   IND,C'G'            INDICATE GOTO MODE                           
         B     DMP8                GET OTHER DUMP INFORMATION                   
*                                                                               
GIGOTO   DC    CL8'GOTO'                                                        
*                                                                               
GOINLX   DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* CAME FROM GOINLIST (NOT INDIRECT, TWA, D-CHAIN, OR LIST OR GOTO)    *         
* TEST FOR SAVE OR RELEASE OF SSBDMPSV FLAGS                          *         
***********************************************************************         
         SPACE 1                                                                
SETSAVE  CH    R1,=H'1'            2 CHARS OR MORE                              
         BL    SETSAVEX                                                         
         EX    R1,*+8              SAVE FLAG FOR THIS DUMP?                     
         BNE   SETSAVEA                                                         
         CLC   FHDA(0),SSSVE                                                    
         MVI   IND,C'F'            INDICATE SAVE MODE                           
         B     DMP8                GET OTHER DUMP INFORMATION                   
*                                                                               
SETSAVEA EX    R1,*+8              RELEASE FLAG FOR THIS DUMP?                  
         BNE   SETSAVEX                                                         
         CLC   FHDA(0),SSREL                                                    
         MVI   IND,C'E'            INDICATE RELEASE MODE                        
         B     DMP8                GET OTHER DUMP INFORMATION                   
*                                                                               
SSSVE    DC    CL8'SAVE'                                                        
SSREL    DC    CL8'RELEASE'                                                     
*                                                                               
SETSAVEX DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* TEST FOR STATUS CHANGE                                              *         
***********************************************************************         
         SPACE 1                                                                
STATUS   CLM   R1,1,=AL1(1)        MUST BE LENGTH 2 OR MORE                     
         BL    STATUSX                                                          
         EX    R1,*+8                                                           
         BNE   STATUSA                                                          
         CLC   FHDA(0),STPRC       PROCESSING DUMP?                             
         MVI   IND,C'B'                                                         
         B     DMP8                                                             
*                                                                               
STATUSA  EX    R1,*+8                                                           
         BNE   STATUSB                                                          
         CLC   FHDA(0),STFIX       FIXED CAUSE OF DUMP?                         
         MVI   IND,C'C'                                                         
         B     DMP8                                                             
*                                                                               
STATUSB  EX    R1,*+8                                                           
         BNE   STATUSX                                                          
         CLC   FHDA(0),STUND       UNDO STATUS SET?                             
         MVI   IND,C'D'                                                         
         B     DMP8                                                             
*                                                                               
STPRC    DC    CL8'PROCESS'                                                     
STUND    DC    CL8'UNDO'                                                        
STFIX    DC    CL8'FIXED'                                                       
*                                                                               
STATUSX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* CAME FROM  SETSAVE (NOT ANY OF THE ABOVE)                                     
* TEST FOR PARAMETER LIST                                                       
***********************************************************************         
         SPACE 1                                                                
PARAM    EX    R1,*+8              PARAMETER LIST?                              
         BNE   PARAMX                                                           
         CLC   FHDA(0),PAPRM                                                    
         MVI   IND,C'P'            INDICATE PARAMETER LIST                      
*                                                                               
         LA    R2,SRVP2H                                                        
         XC    START,START                                                      
         CLI   FHIL,0              INPUT IN 2ND FIELD?                          
         BE    DMP8                NOTHING, NO DISPLACEMENT                     
*                                                                               
         TM    FHII,FHIIHE         VALID HEX?                                   
         BZ    ERR1                NO                                           
         CLI   FHIL,1              REGISTER?                                    
         BE    DMP3                YES, SET  REGADD  ACCORDINGLY                
*                                                                               
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO  HEX->FULL WORD           
         MVC   START,FULL                                                       
         B     DMP8                NO, DISPLACEMENT                             
*                                                                               
PAPRM    DC    CL8'PARAM'                                                       
*                                                                               
PARAMX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CAME FROM  PARAM (NOT ANY OF THE ABOVE)                                       
* TEST FOR DMGR DATASPACE                                                       
***********************************************************************         
         SPACE 1                                                                
DMGR     EX    R1,*+8              DMGR DSPACE?                                 
         BNE   DMGR02                                                           
         CLC   FHDA(0),DMPRM2                                                   
         MVI   IND,C'K'            INDICATE SECOND DMGR BLOCK                   
         B     DMGR04                                                           
*                                                                               
DMGR02   EX    R1,*+8              DMGR DSPACE?                                 
         BNE   DMGRX                                                            
         CLC   FHDA(0),DMPRM1                                                   
         MVI   IND,C'J'            INDICATE FIRST DMGR BLOCK                    
*                                                                               
DMGR04   LA    R2,SRVP2H                                                        
         XC    START,START                                                      
         CLI   FHIL,0              INPUT IN 2ND FIELD?                          
         BE    DMP8                NOTHING, NO DISPLACEMENT                     
*                                                                               
         TM    FHII,FHIIHE         VALID HEX?                                   
         BZ    ERR1                NO                                           
         CLI   FHIL,1              REGISTER?                                    
         BE    DMP3                YES, SET  REGADD  ACCORDINGLY                
*                                                                               
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO  HEX->FULL WORD           
         MVC   START,FULL                                                       
         B     DMP8                NO, DISPLACEMENT                             
*                                                                               
DMPRM1   DC    CL8'DMGR1'                                                       
DMPRM2   DC    CL8'DMGR2'                                                       
*                                                                               
DMGRX    DS    0H                                                               
***********************************************************************         
* CAME FROM  DMGR (NOT ANY OF THE ABOVE)                                        
* TEST FOR WHOAMI                                                               
***********************************************************************         
         SPACE 1                                                                
WHOM     EX    R1,*+8              DMGR DSPACE?                                 
         BE    WHOM2                                                            
         CLC   FHDA(0),WHOMI                                                    
         BE    WHOM2                                                            
         B     WHOMX                                                            
         CLC   FHDA(0),WHOMI                                                    
WHOM2    MVI   IND,C'N'            INDICATE WHOAMI                              
         MVC   START,DUTLA                                                      
         B     DMP8                                                             
*                                                                               
WHOMI    DC    CL8'WHOAMI'                                                      
WHOMI2   DC    CL8'WHOBEI'                                                      
*                                                                               
WHOMX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CAME FROM  SETSAVE (NOT ANY OF THE ABOVE)                                     
* TEST FOR PARAMETER GOTO                                                       
***********************************************************************         
         SPACE 1                                                                
PGOTO    EX    R1,*+8              GOTO PARAMETER LIST?                         
         BNE   PGOTOX                                                           
         CLC   FHDA(0),PGGOTO                                                   
         MVI   IND,C'Q'            INDICATE PARAMETER LIST                      
*                                                                               
         LA    R2,SRVP2H                                                        
         XC    START,START                                                      
         CLI   FHIL,0              INPUT IN 2ND FIELD?                          
         BE    ERR6                NEED A GOTO NUMBER                           
         TM    FHII,FHIINU         VALID NUMERIC?                               
         BZ    ERR6                NO                                           
         CLI   FHIL,1              ONE DIGIT ONLY                               
         BNE   ERR6                                                             
         MVC   FULL(1),FHDA                                                     
         NI    FULL,X'0F'          STRIP ZONE                                   
         CLI   FULL,0                                                           
         BE    ERR6                                                             
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FULL                                                          
         CH    R1,=Y(NDMPPARQ)     ONLY A CERTAIN NUMBER OF PARMS               
         BH    ERR6                                                             
         B     DMP8                NO, DISPLACEMENT                             
*                                                                               
PGGOTO   DC    CL8'PGOTO'                                                       
*                                                                               
PGOTOX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CAME FROM  PARAM (NOT ANY OF THE ABOVE)                                       
* TEST FOR STOP OR START COMMAND TO SET ALL DUMP MASK IN SSB                    
***********************************************************************         
         SPACE 1                                                                
SETMASK  EX    R1,*+8              SET STOP FLAG?                               
         BNE   SETMASKB                                                         
         CLC   FHDA(0),SMSTOP                                                   
         CLI   SRVP2H+(FHIL-FHD),0                                              
         BE    SETMASKA                                                         
         CLC   SRVP2(8),SMOVR      CHECK FOR PASSWORD                           
         BNE   SETMASKA                                                         
         MVI   MASKFLAG,C'Y'       INDICATE MASK SET                            
         B     DMP8                GET OTHER DUMP INFORMATION                   
*                                                                               
SETMASKA NI    SRVP2H+6,X'FF'-X'40'                                             
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         B     ERR1                                                             
*                                                                               
SETMASKB EX    R1,*+8              RELEASE STOP FLAG                            
         BNE   SETMASKX                                                         
         CLC   FHDA(0),SMSTRT                                                   
         MVI   MASKFLAG,C'N'       INDICATE MASK CLEAR                          
         B     DMP8                GET OTHER DUMP INFORMATION                   
*                                                                               
SMSTOP   DC    CL8'STOP'                                                        
SMSTRT   DC    CL8'START'                                                       
SMOVR    DC    CL8'OVERRIDE'                                                    
*                                                                               
SETMASKX DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CAME FROM SETMASK, CHECK FOR FACPAK TABLE KEYWORD                   *         
***********************************************************************         
         SPACE 1                                                                
DMP7     LA    R2,SRVP1H                                                        
         MVI   IND,X'FF'           INDICATE KEYWORD ACCESS                      
         B     DMP7B                                                            
*                                                                               
DMP7A    LA    R2,SRVP2H           **  GOT HERE FROM DMP5C                      
         MVI   IND,X'FE'           INDICATE INDIRECT KEYWORD ACCESS             
*                                                                               
DMP7B    LH    R3,AKEYTAB          TRY TO FIND ENTRY IN FACPAK TABLE            
         AR    R3,RB                                                            
         XR    R1,R1                                                            
         IC    R1,FHIL             INPUT LENGTH                                 
         BCTR  R1,0                -1 FOR CLC IN DMP7C                          
         CLI   FHDA,C''''          QUOTE  E.G. 'ACC = ACC KEYWORD               
         BNE   DMP7C               TO AVOID CONFUSION WITH X'0ACC'              
*                                                                               
         MVC   FHDA(15),FHDA+1     SKIP THE QUOTE                               
         BCTR  R1,0                DECREASE LENGTH BECAUSE OF SKIP              
*                                                                               
DMP7C    EX    R1,*+8                                                           
         BNE   DMP7D                                                            
         CLC   FHDA(0),D7FAC       OPTION LIST HELP KEYWORD?                    
         MVI   IND,C'H'            INDICATE KEYWORD HELP SCREEN                 
         B     DMP8                                                             
*                                                                               
D7FAC    DC    CL8'FACPAK'                                                      
*                                                                               
DMP7D    EX    R1,*+8                                                           
         BE    DMP7E                                                            
         CLC   FHDA(0),0(R3)       CHECK KEYTABLE                               
         LA    R3,KEYTABL(,R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BE    ERR1                CAN'T FIND KEYWORD                           
         B     DMP7D                                                            
*                                                                               
DMP7E    ST    R3,START            SAVE ADDRESS OF TABLE ENTRY                  
         CLI   IND,X'FE'           INDIRECT ?                                   
         BE    DMP7I               YES                                          
*                                                                               
         XC    DISP,DISP                                                        
         MVC   KEYSAVE(8),0(R3)    SAVE NAME FOR MESSAGE                        
         MVI   KEYSAVE+8,C' '                                                   
         MVC   KEYSAVE+9(3),KEYSAVE+8                                           
         CLC   KEYSAVE(8),=CL8'TICTRACE'                                        
         BNE   *+8                                                              
         MVI   IND,X'FD'           TICTRACE INDICATOR (FIRST TIME IN)           
         CLC   KEYSAVE(7),=CL7'TICPOPS'                                         
         BNE   *+8                                                              
         MVI   IND,X'FD'           TICTRACE INDICATOR (FIRST TIME IN)           
*                                                                               
         CLI   KEYSRCE-KEYTAB(R3),X'11' TCB BASE ?                              
         BE    *+12                YES                                          
         CLI   KEYSRCE-KEYTAB(R3),X'F1' TCB ADDR ?                              
         BNE   DMP8                                                             
*                                                                               
         CLI   SRVP2H+(FHIL-FHD),1                                              
         BNE   DMP8                                                             
*                                                                               
         CLI   SRVP2,C'1'                                                       
         BL    DMP7F                                                            
         CLI   SRVP2,C'9'                                                       
         BH    DMP8                                                             
         XR    R1,R1                                                            
         IC    R1,SRVP2            TASK DISP 0-8                                
         SH    R1,=Y(C'1')                                                      
         B     DMP7H                                                            
*                                                                               
DMP7F    CLI   SRVP2,C'A'                                                       
         BL    DMP8                                                             
         CLI   SRVP2,C'I'                                                       
         BH    DMP7G                                                            
         ZIC   R1,SRVP2            TASK DISP 9-17                               
         SH    R1,=Y(C'A')                                                      
         LA    R1,9(R1)                                                         
         B     DMP7H                                                            
*                                                                               
DMP7G    CLI   SRVP2,C'J'                                                       
         BL    DMP8                                                             
         CLI   SRVP2,C'N'                                                       
         BH    DMP8                                                             
         ZIC   R1,SRVP2            TASK DISP 18-22                              
         SH    R1,=Y(C'J')                                                      
         LA    R1,18(R1)                                                        
*                                                                               
DMP7H    MH    R1,=H'536'          EACH TASK ENTRY IS 536 BYTES LONG            
         ST    R1,DISP                                                          
         B     DMP8                                                             
*                                                                               
DMP7I    ZIC   R1,0(R2)            LOOK AT NEXT SCREEN ENTRY                    
         AR    R2,R1                                                            
         CLI   5(R2),0             DISPLACEMENT ENTERED ?                       
         BE    DMP8                NO                                           
         B     DMP4B               VALIDATE IT                                  
         EJECT                                                                  
***********************************************************************         
* GOTO INDICATED ACTION ELSE PROCESS REQUESTED DUMP DISPLAY           *         
***********************************************************************         
         SPACE 1                                                                
DMP8     CLI   HLPFLD,0                                                         
         BNE   HELPOUT                                                          
*                                                                               
         CLI   MASKFLAG,0                                                       
         BE    *+12                                                             
         LA    R2,SRVP1H                                                        
         B     ERR1                                                             
*                                                                               
DMP8B2   CLI   IND,C'M'            AUTO LIST FUNCTION?                          
         BE    LISTING             YES                                          
         CLI   IND,C'L'            LIST FUNCTION?                               
         BE    LISTMODE            YES                                          
         CLI   IND,C'G'            GOTO ENTRY IN LIST FUNCTION?                 
         BE    LISTGOTO            YES                                          
         CLI   IND,7               PF7, GO UP A PAGE?                           
         BE    PFKEY78             YES                                          
         CLI   IND,8               PF8, GO DOWN A PAGE?                         
         BE    PFKEY78             YES                                          
*                                                                               
         CLI   IND,C'F'            SET SAVE FLAG?                               
         BE    *+12                                                             
         CLI   IND,C'E'            SET RELEASE FLAG?                            
         BNE   DMP8B2A                                                          
         BRAS  RE,SVFLAG                                                        
         B     DMP8B2B                                                          
*                                                                               
DMP8B2A  CLI   IND,C'D'            SET UNDO FLAG IN DUMP                        
         BE    *+8                                                              
         CLI   IND,C'C'            SET FIXED FLAG IN DUMP                       
         BE    *+8                                                              
         CLI   IND,C'B'            SET INPROC FLAG IN DUMP                      
         BE    *+8                                                              
         B     DMP8B2C                                                          
*                                                                               
         GOTO1 =A(WRFL),DMCB,(RC),(R9),RR=RELO                                  
*                                                                               
DMP8B2B  XC    SRVP4,SRVP4                                                      
         MVI   SRVP4,C'A'                                                       
         MVI   SRVP4H+(FHIL-FHD),1                                              
         XC    SRVP1,SRVP1                                                      
         MVI   SRVP1H+(FHIL-FHD),0                                              
         GOTO1 =A(NALL),DMCB,(RC),(R9),(RA),RR=RELO                             
         DC    H'0'                                                             
*                                                                               
DMP8B2C  CLI   IND,C'H'            KEYWORD HELP SCREEN?                         
         BNE   DMP8B3                                                           
         GOTO1 =A(FPAK),DMCB,(RC),(R9),(RA),RR=RELO  HELP SCREEN                
         EJECT                                                                  
***********************************************************************         
* CHECK IF WE NEED TO USE 'FF' SCREEN                                 *         
***********************************************************************         
DMP8B3   ZIC   R1,SRVIDH+5         LOAD LENGTH OF SRV REQ                       
         BCTR  R1,0                HAS TO HAVE A $ OR =                         
         BCTR  R1,0                HAS TO BE AT LEAST ONE LETTER                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRVID+1(0),=CL8'ND' 'FF' SCREEN USED?                            
         BE    DMP8B6              YES                                          
         MVC   REGHOLD(L'SRVP1),SRVP4   COPY THE PARAMETERS                     
         MVC   REGHOLD+16(L'SRVP1),SRVP1                                        
         MVC   REGHOLD+32(L'SRVP1),SRVP2                                        
         MVC   REGHOLD+48(L'SRVP1),SRVP3                                        
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(3),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,3          FOR LENGTH OF 5                              
         MVC   SRVP4,REGHOLD       COPY BACK PARAMETERS                         
         MVC   SRVP1,REGHOLD+16                                                 
         MVC   SRVP2,REGHOLD+32                                                 
         MVC   SRVP3,REGHOLD+48                                                 
         OI    SRVP4H+6,X'80'                                                   
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP2H+6,X'80'                                                   
         OI    SRVP3H+6,X'80'                                                   
*                                                                               
DMP8B6   CLI   IND,C'Q'                                                         
         BNE   DMP8B9                                                           
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
*                                                                               
         TM    NDMPFLAG,X'40'      DO WE EVEN HAVE A PARAMETER LIST?            
         BZ    ERR6                NO                                           
*                                                                               
         MVC   FULL(1),SRVP2       R0 = DISP(INTO PARAMETER LIST)               
         NI    FULL,X'0F'                                                       
         ZIC   R0,FULL                                                          
         BCTR  R0,0                                                             
         SLL   R0,2                                                             
*                                                                               
         LA    R1,NDMPPARM         R1 = APPROPRIATE PARAMETER                   
         DROP  R1                                                               
         AR    R1,R0                                                            
         MVC   FULL,0(R1)                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,FULL+1,SRVP1,L'FULL-1                               
         MVI   SRVP1H+5,6                                                       
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'                                                   
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         MVI   IND,0               START OVER WITH NEW PARAMETERS               
         MVI   REGADD,0                                                         
         MVI   NX,0                                                             
         XC    DISP,DISP                                                        
         B     DMP1                GO BACK AS IF WE JUST PRESS ENTER            
*                                                                               
DMP8B9   TM    SAVEDFLG,X'80'      GET REGISTERS FROM DUMP FILE?                
         BZ    DMP8C               YES                                          
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         MVC   REGHOLD(64),NDMPREGS                                             
         DROP  R1                                                               
         B     DMP8D               DISPLAY THE REGISTERS                        
DMP8C    MVC   REGHOLD(36),DR0     COPY DATA IN REGISTERS ON DUMP 0-8           
         MVC   REGHOLD+36(28),DR9                                 9-F           
DMP8D    GOTO1 =A(RDSLCT),DMCB,(RC),(R9),(RA),RR=RELO   SCAN SELECTS            
*                                                                               
         TM    STATFLAG,GOTOBIT    A GOTO                                       
         BNZ   DMP1                GO BACK TO CHECK NEW DISPLACEMENT            
*                                                                               
         GOTO1 =A(REGDISP),DMCB,(RC),(RA),RR=RELO  DISPLAY REGISTERS            
*                                                                               
         CLI   IND,C'P'            PARAMETER LIST WITHOUT A REGISTER            
         BNE   DMP8D1                  OR ADDRESS?                              
         CLI   SRVP2H+5,0                                                       
         BNE   DMP9                                                             
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         TM    NDMPFLAG,X'40'      DO WE HAVE A PARAMETER LIST?                 
         DROP  R1                                                               
         BZ    ERR8                                                             
         GOTO1 =A(PARMSHOW),DMCB,(RC),(R9),(RA),RR=RELO                         
*                                                                               
DMP8D1   CLI   IND,C'T'            TWA?                                         
         BE    DMP8E               YES                                          
         CLI   IND,C'U'            VIEW TWA?                                    
         BE    DMP8E               NO                                           
         CLI   IND,C'V'            VIEW TWA?                                    
         BNE   DMP9                NO                                           
*                                                                               
DMP8E    L     RE,SAVEAREA         GET A(TCB) FOR TWA DIAGNOSE/DISPLAY          
         USING NDMPSAVD,RE                                                      
         L     RE,NDMPATWA         GET A(TWA)                                   
         DROP  RE                                                               
         ST    RE,TWASTRT          SAVE TWA START                               
         MVC   DISP,START          SAVE DISP                                    
         ST    RE,START            SAVE START                                   
*                                                                               
DMP9     LA    RE,INA              PERFORM BASE REGISTER ADD                    
         ST    RE,FULL             SAVE A(INA)                                  
         CLI   REGADD,0            NOTHING TO ADD?                              
         BE    DMP10               YES                                          
         MVC   DUB(1),REGADD                                                    
         NI    DUB,X'0F'           STRIP ZONED DIGIT                            
         SR    R1,R1                                                            
         IC    R1,DUB              REGISTER IN BINARY                           
         CLI   REGADD,C'F'         FIND VALUE OF REGISTER ON DUMP               
         BH    *+8                                                              
         LA    R1,9(R1)                                                         
         SLL   R1,2                4 BYTES/REGISTER                             
         L     R1,REGHOLD(R1)      GET ADDRESS IN REGISTER                      
         CLI   XREGS,C'Y'          EXTENDED REGISTER?                           
         BE    *+8                 YES                                          
         LA    R1,0(R1)            ELSE CLEAR HOB                               
         A     R1,START            ADD ANY DISPLACEMENT                         
         ST    R1,START            SAVE AS START                                
         SR    R6,R6                                                            
         IC    R6,NX               # FOR D-CHAIN (BCT REG)                      
         EJECT                                                                  
*                                                                               
DMP10    L     R3,START            COMPUTE DISK ADDRESS                         
         CLI   IND,X'FD'           IS IT A KEYWORD JOB ?                        
         BL    DMP10A              NO                                           
*                                                                               
         TM    KEYSRCE-KEYTAB(R3),X'F0' REGISTER REQD ?                         
         BZ    DMP101A             YES                                          
         L     R2,DFACS            GET A(DUMPED FACS)                           
         CLI   KEYSRCE-KEYTAB(R3),X'10' SYSTEM FACILITIES BASE ?                
         BE    DMP102              YES                                          
         CLI   KEYSRCE-KEYTAB(R3),X'F0' SYSTEM FACILITIES ADDR ?                
         BE    DMP103              YES                                          
*                                                                               
         L     R2,DTCBE            R2 = A(DUMPED TCB FOR TASK)                  
         CLI   SRVP2H+5,1                                                       
         BNE   DMP101                                                           
         L     R2,DFACS                                                         
         L     R2,VTCB-SYSFACD(R2)                                              
         LA    R2,6(R2)                                                         
         A     R2,DISP                                                          
         XC    DISP,DISP                                                        
*                                                                               
DMP101   CLI   KEYSRCE-KEYTAB(R3),X'11' TCB BASE ?                              
         BE    DMP102              YES                                          
         CLI   KEYSRCE-KEYTAB(R3),X'F1' TCB ADDR ?                              
         BE    DMP103              YES                                          
*                                                                               
         L     R2,DUTLA                                                         
         CLI   KEYSRCE-KEYTAB(R3),X'12' UTL BASE ?                              
         BE    DMP102              YES                                          
         CLI   KEYSRCE-KEYTAB(R3),X'F2' UTL ADDR ?                              
         BE    DMP103              YES                                          
         DC    H'0'                                                             
*                                                                               
DMP101A  XR    R2,R2                                                            
         IC    R2,KEYSRCE-KEYTAB(R3) KEY REGISTER                               
         SLL   R2,2                                                             
         L     R2,REGHOLD(R2)                                                   
*                                                                               
DMP102   L     R3,KEYSRCE-KEYTAB(R3)                                            
         LA    R3,0(R2,R3)                                                      
         ST    R3,START                                                         
         B     DMP10B                                                           
*                                                                               
DMP103   ST    R2,START                                                         
         XC    SRVP1,SRVP1                                                      
         LR    R3,R2                                                            
         GOTO1 VHEXOUT,DMCB,START,DUB,4                                         
         MVC   SRVP1(6),DUB+2                                                   
         MVI   IND,0                                                            
*                                                                               
DMP10A   CLI   IND,C'T'            TWA JOB ?                                    
         BE    DMP10B              YES - DISP HANDLED ELSEWHERE                 
         CLI   IND,C'U'            VIEW TWA JOB ?                               
         BE    DMP10B              YES - DISP HANDLED ELSEWHERE                 
         CLI   IND,C'V'            VIEW TWA JOB ?                               
         BE    DMP10B              YES - DISP HANDLED ELSEWHERE                 
         CLI   IND,C'N'            WHOAMI?                                      
         BE    DMP10B              YES - DISP HANDLED ELSEWHERE                 
         A     R3,DISP             ADD DISP IF ENTERED                          
*                                                                               
DMP10B   CLI   IND,C'J'            IS THIS DMGR1 BLOCK?                         
         BNE   DMP10BA                                                          
         GOTO1 =A(DMGRADDR),RR=RELO                                             
         BNE   ERR2                                                             
         OI    SRVP2H+(FHOI-FHD),FHOICU                                         
         B     DMP10D                                                           
*                                                                               
DMP10BA  CLI   IND,C'K'            IS THIS DMGR2 BLOCK?                         
         BNE   DMP10BB                                                          
         GOTO1 =A(DMGRADD2),RR=RELO                                             
         BNE   ERR2                                                             
         OI    SRVP2H+(FHOI-FHD),FHOICU                                         
         B     DMP10D                                                           
*                                                                               
DMP10BB  C     R3,DXALO            IS ADDRESS VALID XA ADDRESS?                 
         BL    DMP10C                                                           
         C     R3,DXAHI                                                         
         BH    DMP10C                                                           
         GOTO1 =A(XTNDADDR),RR=RELO                                             
         BNE   ERR2                                                             
         B     DMP10D                                                           
*                                                                               
DMP10C   S     R3,DSTRT            START ADDRESS BELOW MINIMUM?                 
         BM    ERR2                YES                                          
*                                                                               
         SRL   R3,RSHIFT           R3 = # OF 8K BLOCKS FROM BEGINNING           
         ST    R3,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
*                                                                               
         SR    R2,R2                                                            
         D     R2,RECTRK           R2 = BLOCK NUM ON TRK MINUS 1                
         STC   R2,ADDR+2           R3 = TRACK NUM AFTER HEADER                  
         LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR                                                     
         AR    R3,R2               R3 = TRK NUM (ABSOLUTE DISK ADDRESS)         
         STH   R3,ADDR                                                          
*                                                                               
         LH    R3,DRECS            R3 = # OF 2K DUMP PAGES IN DUMP FILE         
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)      R3 = # OF 8K RECORDS IN DUMP FILE            
         L     R2,BLKNUM                                                        
         LA    R2,1(R2)                                                         
         SR    R3,R2                                                            
         BM    ERR2                ADDRESS OUT OF RANGE                         
         STH   R3,NBLKS2           USED TO COUNT DOWN BLOCKS READ               
*                                                                               
DMP10D   CLI   IND,C'I'            READ FOR START FOR I MODE                    
         BNE   DMP11                                                            
         MVI   IND,0                                                            
         L     R4,START                                                         
         S     R4,DSTRT                                                         
         SRDL  R4,RSHIFT           DIVIDE BY 8192                               
         SRL   R5,32-RSHIFT        REMAINDER IS # OF BYTES INTO RECORD          
         LA    R5,INA(R5)                                                       
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
         L     R5,0(R5)                                                         
         LA    R5,0(R5)                                                         
         ST    R5,START                                                         
         B     DMP10                                                            
         EJECT                                                                  
***********************************************************************         
* LOOKING AT THE D-CHAIN, GO BACKWARDS BY LOOKING 4 BEYOND THE CHAIN  *         
***********************************************************************         
DMP11    CLI   IND,C'X'            INITIAL READS FOR X MODE                     
         BNE   DMP11A              COULD BE Y MODE, FORWARD PTR                 
         L     R4,START                                                         
         S     R4,DSTRT                                                         
         SRDL  R4,RSHIFT           DIVIDE BY 8192                               
         SRL   R5,32-RSHIFT        REMAINDER IS # OF BYTES INTO RECORD          
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
         LA    R5,INA(R5)          POINT TO THE D-CHAIN                         
         L     R5,4(R5)            GET THE BACKWARD POINTER                     
         LA    R5,0(R5)            CLEAR HOB TO GET THE ADDRESS                 
         ST    R5,START            THAT'S THE START ADDRESS                     
         BCT   R6,DMP10                                                         
         MVI   IND,0                                                            
         B     DMP10                                                            
         EJECT                                                                  
***********************************************************************         
* LOOKING AT THE D-CHAIN, GO FORWARDS BY LOOKING 8 BEYOND THE CHAIN   *         
***********************************************************************         
DMP11A   CLI   IND,C'Y'            INITIAL READS FOR Y MODE                     
         BNE   DMP12                                                            
         L     R4,START                                                         
         S     R4,DSTRT                                                         
         SRDL  R4,RSHIFT           DIVIDE BY 8192                               
         SRL   R5,32-RSHIFT        REMAINDER IS # OF BYTES INTO RECORD          
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
         LA    R5,INA(R5)          POINT TO THE D-CHAIN                         
         CLI   NX,0                LOOK DIRECTLY AT D-CHAIN?                    
         BNE   DMP11B              NO                                           
         L     R5,DRD              LOAD ADDRESS OF D-CHAIN                      
         ST    R5,START            THAT'S THE START ADDRESS = RD                
         B     DMP11C                                                           
DMP11B   L     R5,8(R5)            GET THE FORWARD POINTER                      
         LA    R5,0(R5)            CLEAR HOB TO GET THE ADDRESS                 
         ST    R5,START            THAT'S THE START ADDRESS                     
         BCT   R6,DMP10                                                         
DMP11C   MVI   IND,0                                                            
         B     DMP10                                                            
         EJECT                                                                  
*                                                                               
DMP12    CLI   IND,X'FB'           INITIAL READS KEYWORD MODE                   
         BL    DMP14                                                            
*                                                                               
         CLI   IND,X'FC'           SECOND TIME IN TICTRACE?                     
         BNE   DMP12A                                                           
         GOTO1 =A(TICTRACE),RR=RELO YES, CALL TICTRACE OVERLAY                  
         B     DMPEXT              EXIT AFTER DISPLAY TICTRACE TASKS            
*                                                                               
DMP12A   L     R4,START                                                         
         S     R4,DSTRT                                                         
         SRDL  R4,RSHIFT                                                        
         SRL   R5,32-RSHIFT                                                     
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
         LA    R5,INA(R5)                                                       
         L     R3,0(R5)            ADCON                                        
         LA    R3,0(R3)                                                         
         ST    R3,START                                                         
*                                                                               
         CLI   IND,X'FE'           INDIRECT KEYWORD?                            
         BNE   DMP12B                                                           
         A     R3,DISP             YES                                          
         ST    R3,START                                                         
         XC    DISP,DISP                                                        
         MVI   IND,X'FF'                                                        
         B     DMP10B              GO BACK AND CALCULATE WHERE TO READ          
*                                                                               
DMP12B   CLI   IND,X'FD'           FIRST TIME IN TICTRACE?                      
         BNE   DMP12C                                                           
         LA    R3,88(R3)           YES, TICTRACE IS X'58' INTO TICTOCT          
         CLC   KEYSAVE(7),=C'TICPOPS'                                           
         BNE   *+8                                                              
         AH    R3,=H'4332'                                                      
         ST    R3,START                                                         
         MVI   IND,X'FC'           TICTRACE SECOND TIME AROUND                  
         B     DMP10B                                                           
*                                                                               
DMP12C   XC    SRVP1,SRVP1                                                      
         MVC   SRVP2,SRVP1                                                      
         MVC   SRVP3,SRVP1                                                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,START,DUB,4                                         
         MVC   SRVP1(6),DUB+2                                                   
         MVI   IND,0                                                            
         B     DMP10B              GO BACK AND READ ABSOLUTE ADDRESS            
         EJECT                                                                  
DMP14    L     R4,START            READ DUMP BLOCKS                             
         CLI   IND,C'T'            TWA REQUEST?                                 
         BE    DMP14A              YES - DISP HANDLED ELSWHERE                  
         CLI   IND,C'U'            VIEW TWA REQUEST?                            
         BE    DMP14A              YES - DISP HANDLED ELSWHERE                  
         CLI   IND,C'V'            VIEW TWA REQUEST?                            
         BE    DMP14A              YES - DISP HANDLED ELSWHERE                  
         CLI   IND,C'N'            VIEW WHOAMI DETAILS?                         
         BE    DMP14A              YES - DISP HANDLED ELSWHERE                  
         A     R4,DISP             ADD DISP TO START IF ENTERED                 
         ST    R4,START            STORE UPDATED START                          
*                                                                               
DMP14A   C     R4,DXALO                                                         
         BL    DMP14A0                                                          
         C     R4,DXAHI                                                         
         BH    DMP14A0                                                          
         S     R4,DXALO                                                         
         B     DMP14A0A                                                         
*                                                                               
DMP14A0  S     R4,DSTRT                                                         
DMP14A0A SRDL  R4,RSHIFT           DIVIDE BY 8K                                 
*                                                                               
         SRL   R5,32-RSHIFT        R5 = REMAINDER IS OFFSET INTO INA            
         LA    R5,INA(R5)                                                       
         LA    R2,SRVL1H           LINE ON WHERE TO START PRINTING              
         USING LIND,R2             USE A TEMPLATE                               
         BRAS  RE,READDSK          INITIAL READ                                 
         BNZ   DSKERR                                                           
*                                                                               
         CLI   IND,C'N'                                                         
         BNE   DMP14A0B            SPECIAL CODE FOR =WHOAMI SCREEN              
         GOTO1 =A(WHOAMI),DMCB,(RC),(R9),(RA),RR=RELO                           
         B     DMPEXT                                                           
*                                                                               
DMP14A0B CLI   IND,C'T'                                                         
         BNE   DMP14A1             SPECIAL CODE FOR TWA DISPLAY/DIAGNOZ         
         GOTO1 =A(TWACHK),DMCB,(RC),(R9),(RA),RR=RELO                           
*                                                                               
DMP14A1  CLI   IND,C'U'                                                         
         BE    *+12                                                             
         CLI   IND,C'V'                                                         
         BNE   DMP14A2             SPECIAL CODE FOR SHOWING TWA                 
         GOTO1 =A(TWASHOW),DMCB,(RC),(R9),(RA),RR=RELO                          
*                                                                               
DMP14A2  CLI   IND,C'P'                                                         
         BNE   DMP14A3                                                          
         GOTO1 =A(PARMSHOW),DMCB,(RC),(R9),(RA),RR=RELO                         
*                                                                               
DMP14A3  CLC   =C'STEREO',SRVP3                                                 
         BE    *+14                                                             
         CLC   =C'STEREO',KEYSAVE                                               
         BNE   DMP14A4                                                          
         GOTO1 =A(SOFSHOW),DMCB,(RC),(R9),(RA),RR=RELO                          
*                                                                               
DMP14A4  TM    STATFLAG,SETREGS    SET REGISTER OPTION? D-CHAIN                 
         BZ    DMP14B                                                           
         MVC   REGHOLD+56(8),12(R5)  COPY RE AND RF                             
         MVC   REGHOLD(52),20(R5)  COPY R0-RD                                   
         GOTO1 =A(REGDISP),DMCB,(RC),(RA),RR=RELO  DISPLAY REGISTERS            
         NI    STATFLAG,X'FF'-SETREGS  NOT SET REGISTER ANYMORE                 
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO                              
         XC    SRVP1,SRVP1                                                      
         XC    SRVP2,SRVP2                                                      
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP1H+5,0                                                       
         MVI   SRVP2H+5,0                                                       
         MVI   SRVP3H+5,0                                                       
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    START,START                                                      
         XC    DISP,DISP                                                        
         B     DMP1                                                             
*                                                                               
DMP14B   L     R1,SAVEAREA         POINT TO THE SAVEAREA IN TWAB                
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPCOLS         FIND WHERE TO STORE THE COLUMNS              
         LA    R3,NDMPADDR         FIND WHERE TO STORE THE ADDRESSES            
         DROP  R1                                                               
         CLI   IND,C'S'            SEARCH MODE?                                 
         BNE   DMP14C                                                           
         GOTO1 =A(SEARCH),DMCB,(RC),(RA),RR=RELO                                
         B     DMP1                                                             
*                                                                               
DMP14C   TM    STATFLAG,FOUNDFLG   FOUND AFTER A SEARCH?                        
         BZ    DMP15               NO                                           
         XC    SRVP1,SRVP1         CLEAR DISPLACEMENT                           
         GOTO1 VHEXOUT,DMCB,NXTSTART,SRVP1,4  PUT NXT START ADDRESS IN          
         OI    SRVP1H+6,X'80'      TRANSMIT IT ALSO                             
*                                                                               
DMP15    TM    DMPFLGS1,DMPDSCTD   CALL HABER'S DSECT DISPLAYER?                
         BZ    DMP16               NO                                           
         L     RF,=A(DSECTDSP)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     DMPEXT                                                           
         EJECT                                                                  
DMP16    LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
         CH    R7,=H'16'           MORE THAN 16 THEN CONTINUE PRINTING          
         BH    DMP20                                                            
         LA    RF,INA              HOLD BYTES AT END OF LAST REC                
         A     RF,=A(RLEN-16)                                                   
         MVC   INBACK,0(RF)                                                     
         S     R5,=A(RLEN)                                                      
         XC    INA(16),INA                                                      
         OC    NBLKS2,NBLKS2                                                    
         BNZ   *+12                                                             
         MVI   DONESW,1                                                         
         B     DMP20                                                            
         LH    R0,NBLKS2                                                        
         BCTR  R0,R0                                                            
         STH   R0,NBLKS2                                                        
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
         EJECT                                                                  
DMP20    CLC   1(15,R5),0(R5)                                                   
         BNE   DMP28                                                            
         CLI   SAMIND,1                                                         
         MVI   SAMIND,1                                                         
         BNE   DMP24                                                            
         BCTR  R5,R0                                                            
         CLC   0(1,R5),1(R5)       BYPASS LINE IF SAME AS LAST                  
         LA    R5,1(R5)                                                         
         BE    DMP36                                                            
*                                                                               
DMP24    GOTO1 VHEXOUT,DMCB,(R5),LINCOL1,4                                      
         MVC   LINCOL2,=C'--SAME--'                                             
         MVC   LINHEX(4),0(R5)                                                  
         TR    LINHEX(4),TRTAB                                                  
         MVC   0(16,R6),0(R5)      STORE THE IDENTICAL BYTES                    
         B     DMP32                                                            
*                                                                               
DMP28    MVI   SAMIND,0                                                         
         GOTO1 VHEXOUT,DMCB,(R5),LINCOL4,16                                     
         MVC   LINCOL1,LINCOL4                                                  
         MVC   LINCOL2,LINCOL4+8                                                
         MVC   LINCOL3,LINCOL4+16                                               
         MVC   LINCOL4(8),LINCOL4+24                                            
         MVI   LINCOL4+8,0         BLANK OUT 1 SPACE                            
         MVC   LINCOL4+9(23),LINCOL4+8 (NUMBER TO BLANK OUT) - 1                
         MVC   LINHEX,0(R5)                                                     
         TR    LINHEX,TRTAB                                                     
         MVC   0(16,R6),0(R5)      STORE THE DATA                               
*                                                                               
DMP32    MVC   0(4,R3),START       COPY ADDRESS CONTAINING THE 16 BYTES         
         GOTO1 VHEXOUT,DMCB,START,DUB,4                                         
         MVC   LINSTRT,DUB                                                      
         LA    R6,16(R6)           NEXT QUAD IN COLUMN DATA                     
         LA    R3,4(R3)            NEXT ADDRESS IN ADDRESS DATA                 
         LA    R2,LINNXT           NEXT LINE                                    
         CLI   0(R2),0             BEYOND TWA?                                  
         BE    DMP50               YES, DONE                                    
*                                                                               
DMP36    LA    R4,16               BUMP POINTERS                                
         A     R4,DISP                                                          
         ST    R4,DISP                                                          
         LA    R4,16                                                            
         A     R4,START                                                         
         ST    R4,START                                                         
         LA    R5,16(R5)                                                        
         CLI   DONESW,1            CHECK HAVE LAST REC                          
         BNE   DMP16               NO                                           
         LA    R0,INA              YES - CHECK START OF THIS LINE               
         CR    R5,R0                                                            
         BL    DMP16                                                            
         XC    DISP,DISP           ZERO DISP AT END OF PTTN                     
         EJECT                                                                  
DMP50    MVC   SRVMSG+7(L'COMPMSG),COMPMSG                                      
         CLI   IND,0               CHECK IF ADDR/DISP FORMAT                    
         BNE   DMP55               SKIP IF NOT                                  
         CLI   REGADD,0                                                         
         BNE   DMP55               OTHERWISE FRIG MESSAGE                       
         MVC   SRVMSG+37(12),=CL12'to page'                                     
         TM    STATFLAG,FOUNDFLG   AFTER A FOUND?                               
         BNZ   DMPEXT              EXIT, NO KEY TABLE ENTRY                     
         CLI   KEYSAVE,0                                                        
         BE    DISPOUT                                                          
DMP50A   MVC   SRVMSG+7(12),KEYSAVE                                             
         GOTO1 VCALLOV,DMCB,0,(C'R',X'00000A0D')                                
         L     RF,DMCB                                                          
         LA    R0,L'SRVMSG                                                      
         GOTO1 (RF),DMCB,SRVMSG,(R0)                                            
         B     DISPOUT             AND GO WRITE DISP FIELD                      
DMP55    B     DMPEXT                                                           
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ERR1     MVC   SRVMSG+L'ERRHDR(L'ERRMSG1),ERRMSG1                               
         B     ERRX                                                             
*                                                                               
ERR2     MVC   SRVMSG+L'ERRHDR(L'ERRMSG2),ERRMSG2                               
         LA    R2,SRVP1H                                                        
         B     ERRX                                                             
*                                                                               
ERR3     MVC   SRVMSG+L'ERRHDR(L'ERRMSG3),ERRMSG3                               
         LA    R2,SRVMSG+L'ERRHDR+L'ERRMSG3+1                                   
         EDIT  (B1,DUMPMAX),(3,(R2)),ALIGN=LEFT                                 
         LA    R2,SRVP4H                                                        
         B     ERRX                                                             
*                                                                               
ERR4     MVC   SRVMSG+L'ERRHDR(L'ERRMSG4),ERRMSG4                               
         LA    R2,SRVP3H                                                        
         B     ERRX                                                             
*                                                                               
ERR5     MVC   SRVMSG+L'ERRHDR(L'ERRMSG5),ERRMSG5                               
         LA    R2,SRVP3H                                                        
         B     ERRX                                                             
*                                                                               
ERR6     MVC   SRVMSG+L'ERRHDR(L'ERRMSG6),ERRMSG6                               
         LA    R2,SRVP2H                                                        
         B     ERRX                                                             
*                                                                               
ERR7     MVC   SRVMSG+L'ERRHDR(L'ERRMSG7),ERRMSG7                               
         LA    R2,SRVP2H                                                        
         B     ERRX                                                             
*                                                                               
ERR8     MVC   SRVMSG+L'ERRHDR(L'ERRMSG8),ERRMSG8                               
         LA    R2,SRVP1H                                                        
         B     ERRX                                                             
*                                                                               
MOVERR   MVC   SRVMSG+L'ERRHDR(L'INVMSG),INVMSG                                 
         B     ERRX                                                             
*                                                                               
DSKERR   MVC   SRVMSG+L'ERRHDR(L'DSKMSG),DSKMSG                                 
         B     ERRX                                                             
*                                                                               
LISTERR  MVC   SRVMSG+L'ERRHDR(L'OVFLMSG),OVFLMSG                               
         B     ERRX                                                             
*                                                                               
ADDRERR  MVC   SRVMSG+L'ERRHDR(L'ADDRMSG),ADDRMSG                               
         B     ERRX                                                             
*                                                                               
ERRX     NI    SRVP1H+6,X'FF'-X'40'     UNSET CURSOR                            
         OI    6(R2),X'40'                                                      
         MVC   SRVMSG(L'ERRHDR),ERRHDR                                          
         MVI   SRVMSG+8,C'('                                                    
         MVC   SRVMSG+9(L'SYSNAME),SYSNAME                                      
         LA    RF,SRVMSG+L'SYSNAME+9                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         MVC   SRVMSG+2(1),SYSCH                                                
         B     EXMOD                                                            
*                                                                               
COMPMSG  DC    C'System dump displayed - enter next request'                    
NONEMSG  DC    C'Argument not found - hit enter to continue'                    
ERRMSG1  DC    C'Address expression invalid'                                    
ERRMSG2  DC    C'Address outside permissible range'                             
ERRMSG3  DC    C'Maximum dump number is: '                                      
ERRMSG4  DC    C'Invalid search argument'                                       
ERRMSG5  DC    C'Need an end quote'                                             
ERRMSG6  DC    C'Invalid goto number'                                           
ERRMSG7  DC    C'No address for that entry number'                              
ERRMSG8  DC    C'Parameter list does not exist yet'                             
DSKMSG   DC    C'Disk error on dump file'                                       
INVMSG   DC    C'INVALID INPUT VALUE'                                           
OVFLMSG  DC    C'Surpassed maximum number of entries on list '                  
ADDRMSG  DC    C'Address outside permissible range '                            
ERRHDR   DC    CL15'ED/9999'                                                    
*                                                                               
         SPACE 2                                                                
DMPEXT   DS    0H                  SAVE THE TWA FOR LATER                       
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO                              
         B     EXMOD                                                            
         EJECT                                                                  
***********************************************************************         
* LIST MODE - SELF CONTAINED AND SIMPLE CONTROLLER FOR THE LIST       *         
***********************************************************************         
         USING T15DFED,RA          DSECT FOR LIST MODE                          
LISTING  GOTO1 =A(RDLIST),DMCB,(RC),(R9),(RA),RR=RELO   SCAN SELECTS            
         TM    STATFLAG,GOTOBIT    A GOTO                                       
         BNZ   DMP1                GO BACK TO CHECK NEW DISPLACEMENT            
LISTMODE DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(X'FE',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   LSTMSG(35),=C'List of storage addresses displayed'               
         MVC   LSTID(5),=C'$ND,L'    INDICATE LIST OPTION FOR FACPAK            
         OI    LSTIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   LSTIDH+7,5          FOR LENGTH OF 5                              
         MVC   LSTP4(1),DUMPNUM    DUMP NUMBER COULD HAVE BEEN ERASED           
         OI    LSTP4,X'F0'         MAKE IT EBCDIC NUMERIC                       
         OI    LSTP4H+6,X'80'      TRANSMIT                                     
         MVI   LSTP4H+7,1          FOR LENGTH OF 1                              
         MVC   LSTP1(4),=C'LIST'   INDICATE LIST OPTION FOR LATER               
         OI    LSTP1H+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   LSTP1H+7,4          FOR LENGTH OF 4                              
         NI    LSTP1H+6,X'FF'-X'40'  UNSET CURSOR                               
         OI    LSTSLCTH+6,X'40'    SET IT TO THE SELECT FIELD                   
         LA    R2,LSTNUMBH         FIRST LINE FIRST ENTRY                       
         L     R7,SAVEAREA                                                      
         USING NDMPSAVD,R7                                                      
         CLI   NDMPLCNT,0          ANY ENTRIES AFTER SELECTS?                   
         BE    DMPEXT              NONE                                         
         LA    R4,NDMPLIST         YES, POINT TO THE LIST                       
         ZIC   R3,NDMPLCNT                                                      
         DROP  R7                                                               
LISTMDLP ZIC   R1,0(R2)                                                         
         AR    R2,R1               MOVE OVER TO SELECT FIELD                    
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               MOVE OVER TO ADDR FIELD                      
         GOTO1 VHEXOUT,DMCB,(R4),8(R2),L'LSTADDR/2   PRINT THE ADDRESS          
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               MOVE OVER TO REMARK FIELD                    
         LA    R4,L'LSTADDR/2(R4)  POINT TO THE REMARK                          
         MVC   8(L'LSTRMRK,R2),0(R4)                                            
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               MOVE OVER TO NEXT NUMBERED FIELD             
         LA    R4,L'LSTRMRK(R4)    POINT TO THE NEXT ADDRESS                    
         BCT   R3,LISTMDLP         CONTINUE UNTIL NO MORE                       
         B     DMPEXT              YES                                          
         EJECT                                                                  
         USING T15DFFD,RA                                                       
***********************************************************************         
* GOTO ENTRY IN LIST                                                            
***********************************************************************         
LISTGOTO DS    0H                                                               
         LA    R2,SRVP2H           POINT TO FIELD WITH GOTO NUMBER              
         CLI   5(R2),0             NO GOTO NUMBER?                              
         BE    ERR6                NONE                                         
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    ERR6                NO                                           
         CLI   5(R2),2             2 DIGITS?                                    
         BH    ERR6                ONLY 24 ENTRIES IN LIST                      
         ZIC   R1,5(R2)            LOAD LENGTH                                  
         BCTR  R1,0                -1 FOR PACK                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         LOAD NUMERIC                                 
         CVB   R3,DUB                AND CONVERT TO BINARY                      
         LTR   R3,R3               0?                                           
         BZ    ERR6                YES                                          
         CH    R3,=H'24'           OVER MAXIMUM NUMBER OF ENTRIES?              
         BH    ERR6                YES                                          
         L     R1,SAVEAREA         POINT TO THE TWA 11 SAVE AREA                
         USING NDMPSAVD,R1                                                      
         ZIC   R4,NDMPLCNT         FIND NUMBER OF LIST ENTRIES                  
         LA    R6,NDMPLIST         POINT TO THE LIST                            
         DROP  R1                                                               
         CR    R3,R4               OVER CURRENT NUMBER OF ENTRIES?              
         BH    ERR7                YES                                          
         BCTR  R3,0                OFFSET FROM 1ST                              
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         GOTO1 VHEXOUT,DMCB,1(R6),SRVP1,L'LSTADDR/2-1                           
         MVI   SRVP1H+5,L'LSTADDR-2                                             
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    DISP,DISP                                                        
         B     DMP1                                                             
         EJECT                                                                  
***********************************************************************         
* GOTO UP OR DOWN A PAGE                                              *         
***********************************************************************         
PFKEY78  DS    0H                                                               
         L     R1,SAVEAREA         POINT TO THE TWA 11 SAVE AREA                
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPADDR         POINT TO THE ADDRESSES                       
         DROP  R1                                                               
         CLI   IND,7               PF7? UP A PAGE                               
         BNE   PFKEY8              NO, DOWN A PAGE                              
         L     R1,0(R6)                                                         
         SH    R1,=H'256'          GO BACK 256 BYTES                            
         ST    R1,DUB                                                           
         B     PFKEYOUT            OUTPUT THE ADDRESS                           
PFKEY8   MVC   DUB(4),64(R6)       OUTPUT LAST ADDRESS IN NDMPADDR              
PFKEYOUT GOTO1 VHEXOUT,DMCB,DUB,SRVP1,4                                         
         MVI   SRVP1H+5,8                                                       
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    DISP,DISP                                                        
         B     DMP1                                                             
         EJECT                                                                  
DISPOUT  MVI   HALF,0                                                           
         ICM   R1,15,DISP          IS DISPLACEMENT -VE ?                        
         BNM   DISPOUT2                                                         
         MVI   HALF,C'-'                                                        
         LCR   R1,R1                                                            
         ST    R1,DISP                                                          
DISPOUT2 GOTO1 VHEXOUT,DMCB,DISP,DUB,4                                          
         XC    SRVP2,SRVP2                                                      
         XC    SRVP3,SRVP3                                                      
         LA    R1,DUB                                                           
         LA    R2,SRVP2                                                         
         LA    R3,8                                                             
         CLI   HALF,C'-'                                                        
         BNE   DISPOUT3                                                         
         MVI   0(R2),C'-'                                                       
         LA    R2,1(,R2)                                                        
DISPOUT3 CLI   0(R1),C'0'                                                       
         BNE   DISPOUT4                                                         
         LA    R1,1(,R1)                                                        
         BCT   R3,DISPOUT3                                                      
         MVC   SRVP2(4),DUB                                                     
         B     DISPOUT5                                                         
DISPOUT4 MVC   0(1,R2),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
         BCT   R3,DISPOUT4                                                      
DISPOUT5 NI    SRVP1H+6,X'FF'-X'40' UNSET CURSOR                                
         OI    SRVP3H+6,X'40'                                                   
         B     DMPEXT                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE STATUS FLAG                                                   *         
***********************************************************************         
WRFLAG   LR    R0,RE                                                            
         GOTO1 =A(WRFL),DMCB,(RC),(R9),RR=RELO                                  
         LR    RE,R0                                                            
         BR    RE                                                               
*************************************************************                   
*                CALL GETHELP AND EXIT                      *                   
*************************************************************                   
HELPOUT  NI    SRVP1H+6,X'FF'-X'40'   UNSET THE CURSOR                          
         TWAXC SRVCLRH,PROT=Y                                                   
         LA    R1,HLPIND6                                                       
         CLI   HLPFLD,6            CHECK FOR SEL FIELDS                         
         BNL   HELP005                                                          
         SR    RF,RF                                                            
         IC    RF,HLPFLD           READ HELP FIELD                              
         SLL   RF,2                                                             
         EX    0,HLPIND0(RF)       SELECT WHICH TABLE                           
         B     HELP010                                                          
HLPIND0  DC    XL4'00'             NO FIELD NUMBER                              
         LA    R1,HLPIND1                                                       
         LA    R1,HLPIND2                                                       
         LA    R1,HLPIND3                                                       
         LA    R1,HLPIND4                                                       
         LA    R1,HLPIND5                                                       
*                                                                               
HELP005  L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
         MVI   TIOBCURI,X'01'      ONE PAST                                     
         LA    RE,SRVP3H           P4                                           
         SR    RE,RA                                                            
         STCM  RE,3,TIOBCURD                                                    
         XC    TIOBCURS,TIOBCURS                                                
         DROP  RF                                                               
*                                                                               
HELP010  CLC   0(2,R1),=H'0'                                                    
         BE    INFO2                                                            
         CLC   0(1,R1),IND         MATCH ACTION ???                             
         BE    HELP020                                                          
         CLI   0(R1),X'FF'         FF MATCHES ANY                               
         BE    HELP020                                                          
         LA    R1,2(R1)            NEXT ENTRY                                   
         B     HELP010                                                          
HELP020  MVC   FLAG,1(R1)          FLAG=INDEX                                   
*                                  EXTRA TEXT STUFF                             
HELP025  LA    R1,HELPTAB                                                       
HELP030  CLC   FLAG,0(R1)                                                       
         BNE   HELP040                                                          
         TM    1(R1),X'80'         DDS PANEL ?                                  
         BNO   HELP031                                                          
         TM    HLPFLG,X'80'                                                     
         BNO   HELP040                                                          
HELP031  MVC   HELPNUM,2(R1)                                                    
         MVC   HELPPAG,HLPPAG                                                   
         SR    RF,RF                                                            
         TM    1(R1),X'40'         EXTRA TEXT ?                                 
         BNO   HELP032                                                          
*        L     RF,?                                                             
HELP032  L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),QHDR,(C'B',0),(RF),0               
         DC    H'0'                GETHELP EXITS TO MONITOR                     
HELP040  LA    R1,L'HELPTAB(R1)                                                 
         CLI   0(R1),0                                                          
         BNE   HELP030                                                          
         B     INFO2                                                            
         EJECT                                                                  
*************************************************************                   
*                       INFO MESSAGES                       *                   
*************************************************************                   
INFO2    L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         LA    RF,4                HELP UNAVAILABLE                             
         B     INFOX                                                            
INFOX    XC    DMCB(24),DMCB                                                    
         GOTO1 VGETTXT,DMCB,(RF),0,(C'I',0)                                     
         B     EXMOD                                                            
*                                                                               
*  HELP INDEX 2 BYTES ACTION-INDEX 00=NO ACTION FF=ANY ACTION                   
*                                                                               
HLPIND1  DC    X'FF010000'                                                      
*                                                                               
HLPIND2  DC    X'FF020000'                                                      
*                                                                               
HLPIND3  DC    X'C10DFF030000'                                                  
*                                                                               
HLPIND4  DC    X'0004E204E30EE718E818C922C72CFF07'                              
*                                                                               
HLPIND5  DC    X'0005E205C90FFF07'                                              
*                                                                               
HLPIND6  DC    X'0006C710D310D410FF07'                                          
         EJECT                                                                  
*  FLAGS       X'80'                   DDS ONLY PANEL                           
*              X'40'                   EXTRA TEXT BLOCK IN PQSAVE               
*                                                                               
HELPTAB  DS    0CL3                    INDEX/FLAGS/PANEL                        
         DC    X'01',X'00',X'01'                                                
         DC    X'02',X'00',X'02'                                                
         DC    X'03',X'00',X'03'                                                
         DC    X'04',X'00',X'04'                                                
         DC    X'05',X'00',X'05'                                                
         DC    X'06',X'00',X'06'                                                
         DC    X'07',X'00',X'07'                                                
         DC    X'0D',X'00',X'0D'                                                
         DC    X'0E',X'00',X'0E'                                                
         DC    X'18',X'00',X'18'                                                
         DC    X'22',X'00',X'22'                                                
         DC    X'0F',X'00',X'0F'                                                
         DC    X'10',X'00',X'10'                                                
         DC    X'2C',X'00',X'2C'                                                
         DC    X'00'                                                            
*                                                                               
HELPID   DC    XL10'015DFF00000000000000'                                       
         EJECT                                                                  
*                                                                               
READDSK  LR    R0,RE                                                            
         GOTO1 =A(READ),DMCB,(RC),(R9),RR=RELO                                  
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EXMOD    L     RD,SAVERD                                                        
XIT      XIT1                                                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         B     XIT                                                              
         EJECT                                                                  
         DS    0F                                                               
AKEYTAB  DC    AL2(KEYTAB-NDUMP)                                                
RELO     DS    A                                                                
         LTORG                                                                  
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B8182838485868788894B4B4B4B4B4B'     80-8F                    
         DC    X'4B9192939495969798994B4B4B4B4B4B'     90-9F                    
         DC    X'4B4BA2A3A4A5A6A7A8A94B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GET THE TICTRACE OVERLAY AND THEN RUNS IT                        
***********************************************************************         
TICTRACE NTR1  BASE=*,LABEL=*                                                   
         L     R4,START                                                         
         S     R4,DSTRT                                                         
         SRDL  R4,RSHIFT                                                        
         SRL   R5,32-RSHIFT        OFFSET INTO A(INA)                           
         LA    R5,INA(R5)                                                       
*                                                                               
         LA    R2,SRVL1H           LINE ON WHERE TO START PRINTING              
         USING LIND,R2             USE A TEMPLATE                               
         BRAS  RE,READDSK          INITIAL READ                                 
         BNZ   DSKERR                                                           
*                                                                               
         LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
*                                                                               
         CH    R7,=H'4332'         ENOUGH TO HOLD WHOLE TICTRACE TABLE?         
         BH    TTRCE10             YES                                          
*                                                                               
         LA    R0,INA              COPY WHAT WE HAVE TO BEG(INA)                
         LR    R1,R7                                                            
         LR    RE,R5                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ST    R0,P2               WHERE TO CONTINUE LOADING WITH DADDS         
         LA    R5,INA              R5 SHOULD POINT TO BEGINNING                 
*                                                                               
         OC    NBLKS2,NBLKS2                                                    
         BNZ   *+12                                                             
         MVI   DONESW,1                                                         
         B     TTRCE10                                                          
*                                                                               
         LH    R0,NBLKS2                                                        
         BCTR  R0,R0                                                            
         STH   R0,NBLKS2                                                        
*                                                                               
         SR    RE,RE               GET BLOCK NUMBER FOR TRACK                   
         IC    RE,ADDR+2                                                        
         C     RE,RECTRK           READ BLOCK ON THIS TRACK?                    
         BE    TTRCE01             YES                                          
         LA    RE,1(RE)            NO, BUMP TO NEXT BLOCK(RECORD)               
         STC   RE,ADDR+2           SAVE IT FOR NEXT READ                        
         B     TTRCE02             READ THE RECORD                              
*                                                                               
TTRCE01  ICM   RE,3,ADDR           BUMP TO NEXT TRACK                           
         LA    RE,1(RE)                                                         
         STH   RE,ADDR                                                          
         MVI   ADDR+2,1            SET BLOCK NUMBER TO FIRST ON TRACK           
*                                                                               
TTRCE02  GOTO1 VDADDS,P1           READ RECORD FROM ADDRESS                     
         OC    P3(2),P3            SET CC TO EQUAL IF READ IS OKAY              
         BNZ   DSKERR                                                           
*                                                                               
TTRCE10  L     R7,8(R5)            GET A(CURRENT TICTRACE ENTRY)                
         S     R7,START                                                         
*                                  8(TICTRACE TABLE) IS DISPLACEMENT            
         ST    R7,8(R5)                 INTO TABLE FOR CURRENT ENTRY            
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'01',0),(0,0)  GET PHASE-LIST ADDRESS             
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(R9),(RA),(R5)   RUN TICTRACE OVERLAY             
*                                                                               
TTRCEX   XIT1  ,                                                                
*        LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATE THE RELATIVE BLOCK NUMBER, THE ADDRESS, AND            
* THE NUMBER OF BLOCKS READ FOR THE EXTENDED ARCHITECTURE ADDRESS.              
***********************************************************************         
XTNDADDR NTR1  BASE=*,LABEL=*                                                   
         S     R3,DXALO                                                         
         SRL   R3,11               R3=# OF 2K BLOCK FROM BEGINNING              
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)      R3=# OF 2K RECORDS/READ                      
         ST    R3,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
*                                                                               
         L     R6,VSSB                                                          
         USING SSBD,R6                                                          
         ZICM  RF,ADDRSTR,(3)      XA DUMP IS AFTER REGULAR DUMP                
         AH    RF,DXA1             TRACK WHERE XA DUMP IS IN                    
         BCTR  RF,0                                                             
         STCM  RF,3,ADDRSTR        SAVE TTTTBB00 AS START OF XA DUMP            
*                                                                               
         SR    R2,R2                                                            
         D     R2,RECTRK           R2=REC NUM ON TRK MINUS 1                    
         STC   R2,ADDR+2           R3=TRK NUM AFTER HEADER                      
         LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR                                                     
         AR    R3,R2               R3=TRK NUM (ABSOLUTE DISK ADDRESS)           
         STH   R3,ADDR                                                          
*                                                                               
         LH    R3,DXA2K                                                         
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)                                                   
         L     R2,BLKNUM                                                        
         LA    R2,1(R2)                                                         
         SR    R3,R2                                                            
         BM    XTNDADDX            ADDRESS OUT OF RANGE                         
         STH   R3,NBLKS2           USED TO COUNT DOWN BLOCKS READ               
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
XTNDADDX XIT1  ,                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATE THE RELATIVE BLOCK NUMBER, THE ADDRESS, AND  *         
* THE NUMBER OF BLOCKS READ FOR THE DMGR DATASPACE.                   *         
***********************************************************************         
         SPACE 1                                                                
DMGRADDR NTR1  BASE=*,LABEL=*                                                   
         SRL   R3,11               R3=# OF 2K BLOCK FROM BEGINNING              
         XR    R2,R2                                                            
         D     R2,=A(BLKFCTR)      R3=# OF 2K RECORDS/READ                      
         ST    R3,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
*                                                                               
         L     R6,VSSB                                                          
         USING SSBD,R6                                                          
         XR    RF,RF                                                            
         ICM   RF,3,ADDRSTR        XA DUMP IS AFTER REGULAR DUMP                
         AH    RF,DXA1             TRACK WHERE XA DUMP IS IN                    
         AH    RF,DXA2             TRACK WHERE DMGR DUMP IS IN                  
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         STCM  RF,3,ADDRSTR        SAVE TTTTBB00 AS START OF DMGR DUMP          
*                                                                               
         SR    R2,R2                                                            
         D     R2,RECTRK           R2=REC NUM ON TRK MINUS 1                    
         STC   R2,ADDR+2           R3=TRK NUM AFTER HEADER                      
*        LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR                                                     
         AR    R3,R2               R3=TRK NUM (ABSOLUTE DISK ADDRESS)           
         STH   R3,ADDR                                                          
*                                                                               
         L     R3,DDM12K                                                        
         XR    R2,R2                                                            
         D     R2,=A(BLKFCTR)                                                   
         L     R2,BLKNUM                                                        
         LA    R2,1(R2)                                                         
         SR    R3,R2                                                            
         BM    DMGRADDX            ADDRESS OUT OF RANGE                         
         STH   R3,NBLKS2           USED TO COUNT DOWN BLOCKS READ               
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
DMGRADDX XIT1  ,                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATE THE RELATIVE BLOCK NUMBER, THE ADDRESS, AND  *         
* THE NUMBER OF BLOCKS READ FOR THE DMGR DATASPACE.                   *         
***********************************************************************         
         SPACE 1                                                                
DMGRADD2 NTR1  BASE=*,LABEL=*                                                   
         SRL   R3,11               R3=# OF 2K BLOCK FROM BEGINNING              
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)      R3=# OF 2K RECORDS/READ                      
         ST    R3,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
*                                                                               
         L     R6,VSSB                                                          
         USING SSBD,R6                                                          
         XR    RF,RF                                                            
         ICM   RF,3,ADDRSTR        XA DUMP IS AFTER REGULAR DUMP                
         AH    RF,DXA1             TRACK WHERE XA DUMP IS IN                    
         AH    RF,DXA2             TRACK WHERE DMGR DUMP IS IN                  
         BCTR  RF,0                                                             
         STCM  RF,3,ADDRSTR        SAVE TTTTBB00 AS START OF DMGR DUMP          
*                                                                               
         SR    R2,R2                                                            
         D     R2,RECTRK           R2=REC NUM ON TRK MINUS 1                    
         STC   R2,ADDR+2           R3=TRK NUM AFTER HEADER                      
*        LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR                                                     
         AR    R3,R2               R3=TRK NUM (ABSOLUTE DISK ADDRESS)           
         STH   R3,ADDR                                                          
*                                                                               
         L     R3,DDM22K                                                        
         XR    R2,R2                                                            
         D     R2,=A(BLKFCTR)                                                   
         L     R2,BLKNUM                                                        
         LA    R2,1(R2)                                                         
         SR    R3,R2                                                            
         BM    DMGRAD2X            ADDRESS OUT OF RANGE                         
         STH   R3,NBLKS2           USED TO COUNT DOWN BLOCKS READ               
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
DMGRAD2X XIT1  ,                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE EXTRACTS INFO AND SAVES IN W/S                         *         
***********************************************************************         
         SPACE 1                                                                
EXTRACT  NTR1  BASE=*                                                           
         MVC   ATIA,4(R1)          SAVE A(TIA)                                  
         MVC   ATIOB,28(R1)        SAVE A(TIOB)                                 
*                                                                               
         L     R6,VSSB             EXTRACT SSB INFO                             
         USING SSBD,R6                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   SYSCH,SSBSYSCH                                                   
         MVC   SYSNAME,SSBSYSN4                                                 
         MVI   SRVMSG,C'('                                                      
         MVC   SRVMSG+1(L'SYSNAME),SYSNAME                                      
         LA    RF,SRVMSG+L'SYSNAME+1                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         MVC   MAXTWAL,=H'6144'                                                 
         CLC   RECLEN,=H'6144'                                                  
         BNE   *+10                                                             
         MVC   MAXTWAL,=H'6144'                                                 
*                                                                               
         L     R6,8(R1)            EXTRACT UTL INFO                             
         USING UTLD,R6                                                          
         MVC   TERMNUMB,TNUM                                                    
         MVI   DDS,0                                                            
         TM    TSTAT,TSTATDDS      DDS TERMINAL?                                
         BZ    +8                                                               
         MVI   DDS,DDSTRM          YES SET FLAG                                 
         MVI   TSVCREQ,X'00'       RESET SRV REQ SCREEN TO RECALCULATE          
*                                                                               
         L     R6,12(R1)           EXTRACT COMFACS INFO                         
         USING COMFACSD,R6                                                      
         MVC   VHEXIN,CHEXIN       SAVE A(HEXIN)                                
         MVC   VHEXOUT,CHEXOUT     SAVE A(HEXOUT)                               
         MVC   VDICTATE,CDICTATE   SAVE A(DICTATE)                              
         MVC   VXSORT,CXSORT       SAVE A(XSORT)                                
         MVC   VDATCON,CDATCON     SAVE A(DATCON)                               
         MVC   VGETHELP,CGETHELP   SAVE A(GETHELP)                              
         MVC   VGETTXT,CGETTXT     SAVE A(GETTXT)                               
         MVC   VSCANNER,CSCANNER   SAVE A(SCANNER)                              
         MVC   VDATTIM,CDATTIM     SAVE A(DATTIM)                               
         ST    R6,VCOMFACS         SAVE A(COMFACS)                              
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GET THE DSECT DISPLAY OVERLAY AND THEN RUNS IT                   
***********************************************************************         
DSECTDSP NTR1  BASE=*                                                           
*                                                                               
         LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
*                                                                               
         CH    R7,=H'4000'         ENOUGH TO HOLD A WHOLE RECORD?               
         BH    DDSP10              YES                                          
*                                                                               
         LA    R0,INA              COPY WHAT WE HAVE TO BEG(INA)                
         LR    R1,R7                                                            
         LR    RE,R5                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ST    R0,P2               WHERE TO CONTINUE LOADING WITH DADDS         
         LA    R5,INA              R5 SHOULD POINT TO BEGINNING                 
*                                                                               
         OC    NBLKS2,NBLKS2                                                    
         BNZ   *+12                                                             
         MVI   DONESW,1                                                         
         B     DDSP10                                                           
*                                                                               
         LH    R0,NBLKS2                                                        
         BCTR  R0,R0                                                            
         STH   R0,NBLKS2                                                        
*                                                                               
         SR    RE,RE               GET BLOCK NUMBER FOR TRACK                   
         IC    RE,ADDR+2                                                        
         C     RE,RECTRK           READ BLOCK ON THIS TRACK?                    
         BE    DDSP01              YES                                          
         LA    RE,1(RE)            NO, BUMP TO NEXT BLOCK(RECORD)               
         STC   RE,ADDR+2           SAVE IT FOR NEXT READ                        
         B     DDSP02              READ THE RECORD                              
*                                                                               
DDSP01   ICM   RE,3,ADDR           BUMP TO NEXT TRACK                           
         LA    RE,1(RE)                                                         
         STH   RE,ADDR                                                          
         MVI   ADDR+2,1            SET BLOCK NUMBER TO FIRST ON TRACK           
*                                                                               
DDSP02   GOTO1 VDADDS,P1           READ RECORD FROM ADDRESS                     
         OC    P3(2),P3            SET CC TO EQUAL IF READ IS OKAY              
         BNZ   DSKERR                                                           
*                                                                               
DDSP10   GOTO1 VCALLOV,DMCB,(X'02',0),(0,0)  GET PHASE-LIST ADDRESS             
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(R9),(RA),(R5)   RUN DSECT DISP OVERLAY           
*                                                                               
DDSPX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEARCH MODE                                                         *         
***********************************************************************         
SEARCH   DS    0H                                                               
         NMOD1 0,**SRCH**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         NI    SRVP1H+6,X'FF'-X'40' UNSET CURSOR                                
         OI    SRVL1S1H+6,X'40'    SET AFTER PARAMETER 4                        
         SR    R3,R3               R3 = # OF BYTES READ                         
         XC    SRCHBLCK,SRCHBLCK   MAKE TRT TABLE ALL NULLS                     
         LA    R1,SRCHBLCK         POINT TO THE SEARCH TABLE                    
         ZIC   R4,SRCHSTR          GET VALUE OF FIRST BYTE IN STRING            
         AR    R1,R4               POINT TO THAT BYTE IN TABLE                  
         MVI   0(R1),X'FF'         SEARCH UNTIL WE ENCOUNTER BYTE               
SRCHLP   LA    R6,256              DEFAULT SEARCH BLOCK LENGTH                  
         CLI   DONESW,1            LAST BLOCK ALREADY?                          
         BNE   SRCHLP0             NO                                           
         LA    R6,INA              POINT TO BEGINNING OF RECORD                 
         SR    R6,R5               FIND GAP BETWEEN                             
         ZIC   R1,STRLEN                                                        
         CR    R6,R1               LESS THAN SEARCH STRING LENGTH?              
         BL    SRCHEXIT            YES, THEN EXIT SEARCH ROUTINE                
         B     CONTSRCH            NO, CONTINUE SEARCH                          
SRCHLP0  LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
         CH    R7,=H'256'          MORE THAN 256?                               
         BNL   CONTSRCH            THEN, CONTINUE SEARCH                        
SRCHLP1  LA    RF,INA              HOLD BYTES AT END OF LAST REC                
         A     RF,=A(RLEN-256)     POINT TO LAST 256 BYTES OF BLOCK             
         MVC   INBACKER,0(RF)      COPY 256 BYTES BEFORE BLOCK                  
         S     R5,=A(RLEN)         SET POINTER TO WHERE IT WAS POINTING         
         XC    INA(256),INA        CLEAR OUT FIRST 256 BYTES IN BLOCK           
         OC    NBLKS2,NBLKS2       NUMBER OF BLOCKS LEFT ZERO?                  
         BNZ   SRCHLP2             NO                                           
         MVI   DONESW,1            JUST IN CASE SOMETHING IN MIDDLE             
         B     SRCHLP              ASSUME ZEROS FOLLOW AFTER LAST BLCK          
SRCHLP2  LH    R0,NBLKS2           LOAD NUMBER OF BLOCK LEFT                    
         BCTR  R0,00               DECREMENT                                    
         STH   R0,NBLKS2           AND STORE                                    
         BRAS  RE,READDSK          READ IN A FULL BLOCK                         
         BNZ   DSKERR              ERROR IF PROBLEM                             
         EJECT                                                                  
CONTSRCH SR    R1,R1               POINT TO WHERE START CHAR IS                 
         BCTR  R6,0                -1 FOR TRT                                   
         EX    R6,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R5),SRCHBLCK    R1=A(WHERE START CHAR IS) OR CC=0            
         BZ    FNDNONE             CC=0 MEANS DIDN'T FIND ANYTHING              
         LR    R2,R1               POINT TO THAT BYTE                           
         SR    R2,R5               # OF BYTES INTO 256 BYTE SECTION             
         AR    R3,R2               FIND TOTAL # OF BYTES READ                   
         C     R3,=F'49152'        DID WE READ PASSED 48K?                      
         BH    SRCHEXIT            YES, EXIT                                    
         LR    R5,R1               POINT TO THAT BYTE                           
         LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
         ZIC   R1,STRLEN           CHECK IF WE CAN COMPARE THE STRING           
         CR    R7,R1               GAP LESS THAN SEARCH STRING LENGTH?          
         BL    SRCHLP              YES, NEED ANOTHER READ                       
         BCTR  R1,0                -1 FOR CLC                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRCHSTR(0),0(R5)    COMPARE THE STRING                           
         BE    FOUNDIT             EXACT MATCH                                  
         LA    R5,1(R5)            NO MATCH, POINT OVER FIRST CHAR              
         LA    R3,1(R3)            INCREMENT # OF BYTES READ                    
         C     R3,=F'49152'        COMPARE IF OVER 48K                          
         BH    SRCHEXIT            READ MORE THAN 48K                           
         B     SRCHLP              OTHERWISE CONTINUE SEARCHING                 
FNDNONE  LA    R6,1(R6)            INCREMENT BECAUSE OF BCTR ABOVE              
         AR    R5,R6               POINT OVER THE BLOCK EXAMINED                
         AR    R3,R6               # OF BYTES READ                              
         C     R3,=F'49152'        COMPARE IF OVER 48K                          
         BH    SRCHEXIT            READ MORE THAN 48K                           
         B     SRCHLP              OTHERWISE CONTINUE SEARCHING                 
FOUNDIT  OI    STATFLAG,FOUNDFLG   FOUND THE STRING                             
SRCHEXIT L     R1,START            LOAD STARTING ADDRESS                        
         AR    R1,R3               POINT TO WHERE STRING IS                     
         ST    R1,START            SAVE STARTING ADDRESS                        
         A     R1,=F'1'            SKIP STARTING CHARACTER                      
         ST    R1,NXTSTART         SAVE NEXT STARTING ADDRESS                   
         XC    SRVP1,SRVP1         CLEAR DISPLACEMENT                           
         GOTO1 VHEXOUT,DMCB,START,SRVP1,4  PUT START ADDRESS IN                 
         OI    SRVP1H+6,X'80'      TRANSMIT IT ALSO                             
         XC    SRVP2,SRVP2         CLEAR ANYTHING IN PARAMETER 3                
         OI    SRVP2H+6,X'80'                                                   
         MVC   SRVMSG+7(L'NONEMSG),NONEMSG  NOT FOUND MESSAGE                   
         TM    STATFLAG,FOUNDFLG   FOUND THE STRING?                            
         BZ    EXMOD               NO, THEN EXIT                                
         XC    SRVMSG+7(L'SRVMSG-7),SRVMSG+7                                    
         L     R1,START            LOAD UP THE START ADDRESS                    
         SRA   R1,2                CLEAR BITS 0 & 1                             
         SLA   R1,2                SO ADDRESS IS ALIGNED ON FULL WORD           
         ST    R1,START              16 BYTES BEFORE IT                         
         LR    R0,R1                                                            
         SH    R1,=H'16'           SEE IF WE CAN SHOW CONTEXT OF STRING         
         C     R0,DXALO                                                         
         BL    SRCH10                                                           
         C     R0,DXALO                                                         
         BH    SRCH10                                                           
         S     R1,DXALO                                                         
         B     SRCH15                                                           
SRCH10   S     R1,DSTRT            SUBTRACT IT WITH THE LOWER BOUND             
SRCH15   BNM   NOTLBND             NOT BELOW LOWER BOUND                        
         OI    SRVL1H+1,X'08'      OTHERWISE, HIGHLIGHT FIRST LINE              
         OI    SRVL1H1H+1,X'08'                                                 
         OI    SRVL1H2H+1,X'08'                                                 
         OI    SRVL1H3H+1,X'08'                                                 
         OI    SRVL1H4H+1,X'08'                                                 
         B     SHOWADDR                                                         
NOTLBND  OI    SRVL2H+1,X'08'      HIGHLIGHT SECOND LINE                        
         OI    SRVL2H1H+1,X'08'                                                 
         OI    SRVL2H2H+1,X'08'                                                 
         OI    SRVL2H3H+1,X'08'                                                 
         OI    SRVL2H4H+1,X'08'                                                 
         LR    R1,R0                                                            
         SH    R1,=H'16'           16 BYTES BEFORE IT                           
         ST    R1,START                                                         
SHOWADDR GOTO1 VHEXOUT,DMCB,START,SRVP1,4  ADDRESS 16 BYTES FROMSTR             
SRCHXT1  MVI   SRVP1H+5,8                                                       
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         MVI   SRVP2H+5,0                                                       
         MVI   SRVP3H+5,0                                                       
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    DISP,DISP                                                        
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE REGISTERS ON THE SCREEN                                 *         
***********************************************************************         
REGDISP  DS    0H                                                               
         NMOD1 0,**DREG**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     RA,4(R1)            A(TWA)                                       
         LA    R3,REGHOLD          POINT TO A COPY OF DATA IN REGS              
         LA    R4,SRVRGS1          POINT TO SCREEN FOR REGS 0-7                 
         LA    R5,8                8 REGISTERS/LINE                             
         LA    R2,2                2 LINES                                      
         L     RF,VHEXOUT          ADDRESS OF HEXOUT ROUTINE                    
         LA    R0,4                LENGTH = 4                                   
         ST    R0,DMCB+8           STICK INTO PARAMETER LIST                    
*                                                                               
REGDISP2 GOTO1 (RF),DMCB,(R3),(R4)    GOTO1 HEXOUT,DMCB,REG,SCREEN,4            
         LA    R3,4(R3)            NEXT REGISTER                                
         LA    R4,9(R4)            NEXT SCREEN POSITION                         
         BCT   R5,REGDISP2         CONTINUE UNTIL ALL 8 ARE DONE                
         LA    R4,SRVRGS2          2ND REGISTER LINE ON SCREEN                  
         LA    R5,8                8 MORE REGISTERS                             
         BCT   R2,REGDISP2         CONTINUE UNTIL SECOND LINE DONE              
*                                                                               
         LA    R3,DPSW             DATA IN PSW (PROGRAM STATUS WORD)            
         LA    R4,SRVPSW+1         POSITION IT ON SCREEN                        
         GOTO1 VHEXOUT,DMCB,(R3),(R4),8  SEND IT TO SCREEN                      
         MVC   SRVPSW(8),SRVPSW+1  17 SPACES ON SCREEN, 16 DIGITS               
         MVI   SRVPSW+8,0          SPACE BETWEEN THE 2 WORDS IN PSW             
*                                                                               
REGDD    MVC   SRVDAT(8),=C'TIME===>' DISPLAY DATE IF NOT TODAY                 
         L     R4,START            SAVE DUMP START POINTER                      
         L     R3,DFACS            GET INFO FROM DUMP FILE SSB                  
         LA    R5,VSSB-SYSFACD                                                  
         LA    R3,0(R3)                                                         
         AR    R3,R5                                                            
         ST    R3,START                                                         
         GOTO1 =A(RDADR),DMCB,(RC),(R9),RR=RELO                                 
         BNZ   REGDDX                                                           
         L     R3,START            POINTER TO A(SSB) IN DUMP FILE               
         LA    R3,0(R3)                                                         
         L     R3,0(R3)                                                         
         LA    R3,0(R3)                                                         
         ST    R3,START                                                         
         GOTO1 =A(RDADR),DMCB,(RC),(R9),RR=RELO                                 
         BNZ   REGDDX                                                           
         L     R3,START            RETURNS A(SSB) IN CORE BUFFER                
         LA    R3,0(R3)                                                         
         USING SSBD,R3                                                          
         GOTO1 VDATCON,DMCB,(X'05',0),(X'03',TODAY)                             
         CLC   TODAY,SSBDATEB      COMPARE SSB DUMP DATE WITH TODAY             
         BE    REGDDX                                                           
         MVC   SRVDAT,SSBDATE      IF DIFFERENT SHOW DUMP DATE                  
REGDDX   ST    R4,START            RESTORE DUMP START POINTER                   
         DROP  R3                                                               
*                                                                               
REGDT    GOTO1 VDATTIM,DMCB,(X'81',DTIM),WORK                                   
         MVC   SRVTIM+0(2),WORK+8                                               
         MVC   SRVTIM+3(2),WORK+10                                              
         MVC   SRVTIM+6(2),WORK+12                                              
         MVI   SRVTIM+2,C'.'                                                    
         MVI   SRVTIM+5,C'.'                                                    
*                                                                               
         L     R1,DPSW+4           PSW-RB                                       
         LA    R1,0(R1)            ADDRESS OF NEXT INSTRUCTION                  
         L     R4,DRB                                                           
         LA    R4,0(R4)            CONTENTS OF BASE REGISTER FROM DUMP          
         SR    R1,R4               OFFSET OF NEXT INTRUCTION                    
         ST    R1,DUB                                                           
         GOTO1 VHEXOUT,DMCB,DUB,SRVDSP,4,=C'TOG'  SEND TO SCREEN                
*                                                                               
         CLI   DINDIC,C'I'         LOOP INDICATOR                               
         BNE   *+14                                                             
         MVC   SRVLOOP(8),=C'**LOOP**'                                          
         B     XIT                                                              
*                                                                               
         CLI   DRSN,X'FF'          SPIE DUMP?                                   
         BNE   REGDT06             NO - ESTAE                                   
*                                                                               
         LA    RF,SOCTAB                                                        
REGDT02  CLI   0(RF),255                                                        
         BE    REGDT06                                                          
         CLC   DRSN+2(1),0(RF)                                                  
         BE    REGDT04                                                          
         LA    RF,L'SOCTAB(RF)                                                  
         B     REGDT02                                                          
*                                                                               
REGDT04  MVC   SRVLOOP(8),1(RF)                                                 
         B     XIT                                                              
*                                                                               
REGDT06  XC    SRVLOOP,SRVLOOP                                                  
         XC    FULL,FULL                                                        
         XC    DUB,DUB                                                          
         MVC   FULL(L'DRSN),DRSN                                                
         GOTO1 VHEXOUT,DMCB,FULL,DUB,L'DRSN,0                                   
         MVI   SRVLOOP,C'S'                                                     
         MVC   SRVLOOP+1(3),DUB                                                 
         MVI   SRVLOOP+4,C'U'                                                   
         MVC   SRVLOOP+5(3),DUB+3                                               
         B     XIT                                                              
         SPACE 2                                                                
SOCTAB   DS    0XL10                                                            
         DC    XL1'01',CL8'Op-code '                                            
         DC    XL2'02',CL8'Priv Ins'                                            
         DC    XL2'03',CL8'Execute '                                            
         DC    XL2'04',CL8'Protectn'                                            
         DC    XL2'05',CL8'Address '                                            
         DC    XL2'06',CL8'Specn   '                                            
         DC    XL2'07',CL8'Data    '                                            
         DC    XL2'09',CL8'Int div '                                            
         DC    XL2'0B',CL8'Dec div '                                            
         DC    XL2'0C',CL8'XP oflow'                                            
         DC    XL2'0F',CL8'Flt div '                                            
         DC    XL2'FFFF'                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERTS EBCDIC HEX (R2-FIELD HDR) INTO A FULL WORD (FULL)          *         
***********************************************************************         
MOVHEX   DS    0H                                                               
         NMOD1 0,**MHEX**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     RA,4(R1)            A(TWA)                                       
         L     R2,8(R1)            A(FIELD HEADER)                              
         USING FHD,R2                                                           
         CLI   FHIL,8              LARGER THAN A FULL WORD?                     
         BH    MOVERR              CAN'T FIT THEN                               
         SR    R1,R1                                                            
         IC    R1,5(R2)            LENGTH OF HEX                                
         LA    R3,8                                                             
         SR    R3,R1               R3 = # OF DIGITS TO RIGHT JUSTIFY            
         LA    R3,DUB(R3)          WHERE TO RIGHT JUSTIFY INTO DUB              
         MVI   DUB,C'0'            ZERO OUT DUB WITH EBCDIC '0'                 
         MVC   DUB+1(7),DUB                                                     
         BCTR  R1,R0               -1 FOR MVC                                   
         EX    R1,*+4                                                           
         MVC   0(0,R3),8(R2)       OVERLAY TO GET 0'S IN FRONT                  
*                                                                               
         GOTO1 VHEXIN,DMCB,DUB,FULL,8  CONVERT A FULL WORD                      
*                                                                               
MOVEXIT  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SCAN THROUGH SELECT FIELDS FOR AN OPTION IN LIST MODE               *         
***********************************************************************         
         USING T15DFED,RA                                                       
RDLIST   DS    0H                                                               
         NMOD1 0,**RLST**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            TWA                                          
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         ZIC   R4,NDMPLCNT         THE NUMBER OF LIST ITEMS                     
         DROP  R1                                                               
         LA    R2,LSTNUMBH         FIRST LINE                                   
         XR    R3,R3               SELECT FIELD COUNTER                         
RDLSTLP1 CR    R3,R4               WENT BEYOND NUMBER OF ITEMS IN LIST?         
         BNL   RDLSTLP2            YES                                          
         CLI   0(R2),0             WENT BEYOND TWA?                             
         BE    RDLISTNN            YES                                          
         ZIC   R5,0(R2)            MOVE OVER TO SELECT FIELD                    
         AR    R2,R5                                                            
         LA    R3,1(R3)            THE ? SELECT FIELD (1ST, 2ND...)             
         CLI   8(R2),C'D'          DELETE ITEM?                                 
         BE    RDLISTD             YES                                          
         CLI   8(R2),C'G'          GOTO ADDRESS?                                
         BE    RDLISTG             YES                                          
         CLI   8(R2),C'X'          GOTO EXTENDED ADDRESS?                       
         BE    RDLISTX             YES                                          
         B     RDLISTC             CHANGE WHATEVER                              
         EJECT                                                                  
***********************************************************************         
* SCAN THROUGH ADDRESSES THAT HAVE NOT BEEN ADDED YET                 *         
***********************************************************************         
RDLSTLP2 CLI   0(R2),0             WENT BEYOND TWA?                             
         BE    RDLISTNN            YES                                          
         ZIC   R5,0(R2)            MOVE OVER TO SELECT FIELD                    
         AR    R2,R5                                                            
         CLI   8(R2),C'D'          DELETE ITEM?                                 
         BE    RDLST2NX            YES                                          
         CLI   8(R2),C'G'          GOTO ADDRESS?                                
         BE    RDLISTG2            YES                                          
         CLI   8(R2),C'X'          GOTO EXTENDED ADDRESS?                       
         BE    RDLISTX2            YES                                          
         B     RDLISTA             ADD WHATEVER                                 
RDLST2NX ZIC   R5,0(R2)            SKIP TO ADDRESS                              
         AR    R2,R5                                                            
         ZIC   R5,0(R2)            SKIP TO REMARK                               
         AR    R2,R5                                                            
         ZIC   R5,0(R2)            SKIP TO NEXT NUMBERED FIELD                  
         AR    R2,R5                                                            
         B     RDLSTLP2            GO BACK UNTIL END OF TWA                     
         EJECT                                                                  
***********************************************************************         
* ADD ANY ADDRESSES OR REMARKS IN THE LISTING MANUALLY INPUTTED                 
***********************************************************************         
RDLISTA  DS    0H                                                               
         ZIC   R5,0(R2)            SKIP TO ADDRESS                              
         AR    R2,R5                                                            
         CLI   5(R2),0             NOTHING IN ADDRESS?                          
         BE    RDLISTA0            NOTHING TO ADD                               
         TM    4(R2),X'02'         VALID HEX?                                   
         BZ    RDLISTA0            NO                                           
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPLIST         POINT TO THE FIRST DISPLACEMENT              
         ZIC   R3,NDMPLCNT         POINTER TO NEXT AVAILABLE                    
         DROP  R1                                                               
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ADDRESS WE CHANGED              
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO                           
         MVC   0(4,R6),FULL        GET THE ADDRESS INPUTTED                     
         ZIC   R5,0(R2)            SKIP TO REMARK                               
         AR    R2,R5                                                            
         LA    R6,L'LSTADDR/2(R6)  POINT TO THE COMMENT WE CHANGED              
         MVC   0(L'LSTRMRK,R6),8(R2)  COPY THE REMARK                           
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         ZIC   R3,NDMPLCNT         POINTER TO NEXT AVAILABLE                    
         LA    R3,1(R3)                                                         
         STC   R3,NDMPLCNT         ADDED ONE                                    
         DROP  R1                                                               
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         B     RDLISTA1            GO DIRECTLY TO NEXT SELECT FIELD             
RDLISTA0 ZIC   R5,0(R2)            SKIP TO REMARK FIELD                         
         AR    R2,R5                                                            
RDLISTA1 ZIC   R5,0(R2)            SKIP TO NEXT NUMBERED FIELD                  
         AR    R2,R5                                                            
         B     RDLSTLP2            ONLY FOR THOSE INPUTTED MANUALLY             
         EJECT                                                                  
***********************************************************************         
* CHANGE ANY ADDRESSES OR REMARKS IN THE LISTING                      *         
***********************************************************************         
RDLISTC  DS    0H                                                               
         ZIC   R5,0(R2)            SKIP TO ADDRESS                              
         AR    R2,R5                                                            
         LR    R0,R3               SAVE THE COUNT FOR LATER                     
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPLIST         POINT TO THE FIRST DISPLACEMENT              
         DROP  R1                                                               
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ADDRESS WE CHANGED              
         CLI   5(R2),0             NOTHING IN ADDRESS?                          
         BE    RDLISTC0                                                         
         TM    4(R2),X'02'         VALID HEX?                                   
         BZ    RDLISTC0                                                         
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO                           
         MVC   0(4,R6),FULL        GET THE ADDRESS INPUTTED                     
RDLISTC0 ZIC   R5,0(R2)            SKIP TO REMARK                               
         AR    R2,R5                                                            
         LA    R6,L'LSTADDR/2(R6)  POINT TO THE COMMENT WE CHANGED              
         CLI   5(R2),0             NOTHING IN REMARK?                           
         BE    RDLISTC1                                                         
         MVC   0(L'LSTRMRK,R6),8(R2)  COPY THE REMARK                           
RDLISTC1 GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         ZIC   R5,0(R2)            SKIP TO NEXT NUMBERED FIELD                  
         AR    R2,R5                                                            
         LR    R3,R0               THE COUNT THAT WAS SAVED                     
         B     RDLSTLP1                                                         
         EJECT                                                                  
***********************************************************************         
* DELETE THE ITEM IN THE LISTING                                      *         
***********************************************************************         
RDLISTD  DS    0H                                                               
         LR    R0,R3               SAVE THE COUNT FOR LATER                     
         BCTR  R3,0                DISPLACEMENT FROM FIRST ITEM                 
         L     R5,SAVEAREA                                                      
         USING NDMPSAVD,R5                                                      
         ZIC   R1,NDMPLCNT         LOAD UP THE COUNT BEFORE THE DELETE          
         BCTR  R1,0                LESS ONE                                     
         STC   R1,NDMPLCNT         STORE IT BACK                                
         LA    R6,NDMPLIST         POINT TO THE FIRST ITEM                      
         DROP  R5                                                               
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         LR    R3,R0               RESTORE THE COUNT AGAIN                      
RDLSTDLP CR    R3,R1               HAVE TO MOVE ANY MORE?                       
         BH    RDLSTDXT            EXIT FROM THE DELETE                         
* MOVE NEXT ENTRY TO CURRENT ENTRY                                              
         MVC   0(L'NDMPLIST,R6),L'NDMPLIST(R6)                                  
         LA    R6,L'NDMPLIST(R6)               NEXT ENTRY                       
         LA    R3,1(R3)            DECREASE NUMBER TO MOVE                      
         B     RDLSTDLP            MOVE REST                                    
RDLSTDXT GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         ZIC   R5,0(R2)            SKIP TO ADDRESS                              
         AR    R2,R5                                                            
         ZIC   R5,0(R2)            SKIP TO REMARK                               
         AR    R2,R5                                                            
         ZIC   R5,0(R2)            SKIP TO NEXT NUMBERED FIELD                  
         AR    R2,R5                                                            
         LR    R3,R0               THE COUNT THAT WAS SAVED                     
         BCTR  R3,0                ONE LESS ITEM ON LIST                        
         L     R5,SAVEAREA                                                      
         USING NDMPSAVD,R5                                                      
         ZIC   R4,NDMPLCNT         NEW COUNT OF ITEMS                           
         DROP  R5                                                               
         B     RDLSTLP1                                                         
         EJECT                                                                  
***********************************************************************         
* GOTO THE ADDRESS USER SELECTED IN THE LISTING                       *         
***********************************************************************         
         USING T15DFFD,RA                                                       
RDLISTG  DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(3),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,3          FOR LENGTH OF 3                              
         MVC   SRVP4(1),DUMPNUM    DUMP NUMBER COULD HAVE BEEN ERASED           
         OI    SRVP4,X'F0'         MAKE IT EBCDIC NUMERIC                       
         OI    SRVP4H+6,X'80'      TRANSMIT                                     
         OI    SRVP4H+4,X'08'      VALID NUMERIC                                
         MVI   SRVP4H+5,1          FOR LENGTH OF 1                              
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPLIST         POINT TO THE FIRST DISPLACEMENT              
         DROP  R1                                                               
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         GOTO1 VHEXOUT,DMCB,1(R6),SRVP1,L'LSTADDR/2-1                           
         MVI   SRVP1H+5,L'LSTADDR-2                                             
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS  CAN'T SET REGISTERS                      
         B     RDLSEXIT                                                         
         EJECT                                                                  
***********************************************************************         
* GOTO THE EXTENDED ADDRESS USER SELECTED IN THE LISTING                        
***********************************************************************         
         USING T15DFFD,RA                                                       
RDLISTX  DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(3),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,3          FOR LENGTH OF 3                              
         MVC   SRVP4(1),DUMPNUM    DUMP NUMBER COULD HAVE BEEN ERASED           
         OI    SRVP4,X'F0'         MAKE IT EBCDIC NUMERIC                       
         OI    SRVP4H+6,X'80'      TRANSMIT                                     
         OI    SRVP4H+4,X'08'      VALID NUMERIC                                
         MVI   SRVP4H+5,1          FOR LENGTH OF 1                              
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPLIST         POINT TO THE FIRST DISPLACEMENT              
         DROP  R1                                                               
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         GOTO1 VHEXOUT,DMCB,(R6),SRVP1,L'LSTADDR/2                              
         MVI   SRVP1H+5,L'LSTADDR                                               
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS  CAN'T SET REGISTERS                      
         B     RDLSEXIT                                                         
         EJECT                                                                  
***********************************************************************         
* GOTO THE ADDRESS USER INPUTTED MANUAL INTO THE LISTING              *         
***********************************************************************         
RDLISTG2 DS    0H                                                               
         ZIC   R5,0(R2)            SKIP TO ADDRESS                              
         AR    R2,R5                                                            
         CLI   5(R2),0             NOTHING IN ADDRESS?                          
         BE    RDLISTA0            NOTHING TO ADD                               
         TM    4(R2),X'02'         VALID HEX?                                   
         BZ    RDLISTA0            NO                                           
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPLIST         POINT TO THE FIRST DISPLACEMENT              
         ZIC   R3,NDMPLCNT         POINTER TO NEXT AVAILABLE                    
         DROP  R1                                                               
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ADDRESS WE CHANGED              
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO                           
         MVC   0(4,R6),FULL        GET THE ADDRESS INPUTTED                     
         ZIC   R5,0(R2)            SKIP TO REMARK                               
         AR    R2,R5                                                            
         MVC   L'LSTADDR/2(L'LSTRMRK,R6),8(R2)  COPY THE REMARK                 
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         ZIC   R3,NDMPLCNT         POINTER TO NEXT AVAILABLE                    
         LA    R3,1(R3)                                                         
         STC   R3,NDMPLCNT         ADDED ONE                                    
         DROP  R1                                                               
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(3),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,3          FOR LENGTH OF 3                              
         MVC   SRVP4(1),DUMPNUM    DUMP NUMBER COULD HAVE BEEN ERASED           
         OI    SRVP4,X'F0'         MAKE IT EBCDIC NUMERIC                       
         OI    SRVP4H+6,X'80'      TRANSMIT                                     
         OI    SRVP4H+4,X'08'      VALID NUMERIC                                
         MVI   SRVP4H+5,1          FOR LENGTH OF 1                              
         GOTO1 VHEXOUT,DMCB,1(R6),SRVP1,L'LSTADDR/2-1                           
         MVI   SRVP1H+5,L'LSTADDR-2                                             
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS  CAN'T SET REGISTERS                      
         B     RDLSEXIT                                                         
         EJECT                                                                  
***********************************************************************         
* GOTO THE EXTENDED ADDRESS USER INPUTTED MANUAL INTO THE LISTING               
***********************************************************************         
RDLISTX2 DS    0H                                                               
         ZIC   R5,0(R2)            SKIP TO ADDRESS                              
         AR    R2,R5                                                            
         CLI   5(R2),0             NOTHING IN ADDRESS?                          
         BE    RDLISTA0            NOTHING TO ADD                               
         TM    4(R2),X'02'         VALID HEX?                                   
         BZ    RDLISTA0            NO                                           
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPLIST         POINT TO THE FIRST DISPLACEMENT              
         ZIC   R3,NDMPLCNT         POINTER TO NEXT AVAILABLE                    
         DROP  R1                                                               
         MH    R3,=Y(L'NDMPLIST)                                                
         AR    R6,R3               POINT TO THE ADDRESS WE CHANGED              
         GOTO1 =A(MOVHEX),DMCB,(RC),(RA),(R2),RR=RELO                           
         MVC   0(4,R6),FULL        GET THE ADDRESS INPUTTED                     
         ZIC   R5,0(R2)            SKIP TO REMARK                               
         AR    R2,R5                                                            
         MVC   L'LSTADDR/2(L'LSTRMRK,R6),8(R2)  COPY THE REMARK                 
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         ZIC   R3,NDMPLCNT         POINTER TO NEXT AVAILABLE                    
         LA    R3,1(R3)                                                         
         STC   R3,NDMPLCNT         ADDED ONE                                    
         DROP  R1                                                               
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(3),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,3          FOR LENGTH OF 3                              
         MVC   SRVP4(1),DUMPNUM    DUMP NUMBER COULD HAVE BEEN ERASED           
         OI    SRVP4,X'F0'         MAKE IT EBCDIC NUMERIC                       
         OI    SRVP4H+6,X'80'      TRANSMIT                                     
         OI    SRVP4H+4,X'08'      VALID NUMERIC                                
         MVI   SRVP4H+5,1          FOR LENGTH OF 1                              
         GOTO1 VHEXOUT,DMCB,(R6),SRVP1,L'LSTADDR/2                              
         MVI   SRVP1H+5,L'LSTADDR                                               
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         XC    REGADD,REGADD       START OVER WITH NEW PARAMETERS               
         XC    IND,IND                                                          
         XC    NX,NX                                                            
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS  CAN'T SET REGISTERS                      
         B     RDLSEXIT                                                         
* NONE SELECTED                                                                 
RDLISTNN NI    STATFLAG,X'FF'-GOTOBIT  TURN OFF GOTO BIT                        
* EXIT STEP                                                                     
RDLSEXIT B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCAN THROUGH SELECT FIELDS FOR AN OPTION                            *         
***********************************************************************         
RDSLCT   DS    0H                                                               
         NMOD1 0,**RSEL**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            TWA                                          
*                                                                               
         LA    R2,SRVL1H           FIRST LINE                                   
         XR    R3,R3               SELECT FIELD COUNTER                         
*                                                                               
         CLI   SRVP1,C'T'          TWA  OR  PARAMETER LIST?                     
         BE    RDSLCTNN                                                         
         CLI   SRVP1,C'P'                                                       
         BE    RDSLCTNN            YES, EXIT ROUTINE                            
*                                                                               
         TM    STATFLAG,TWA11BIT   TWA 11 DOESN'T MATCH?                        
         BZ    RDSLCTNN            YES, EXIT ROUTINE                            
*                                                                               
RDSLCTLP CLI   0(R2),0             WENT BEYOND TWA?                             
         BE    RDSLCTNN            YES                                          
*                                                                               
         TM    1(R2),X'20'         LOOKING AT PROTECTED FIELD?                  
         BNZ   RDSLCTNX            THEN LOOK AT NEXT FIELD                      
*                                                                               
         LA    R3,1(R3)            THE Nth SELECT FIELD (1st, 2nd...)           
         CLI   5(R2),0             NOTHING IN THIS SELECT FIELD?                
         BE    RDSLCTNX            YES, CHECK NEXT FIELD                        
*                                                                               
         CLI   8(R2),C'A'          ADD ADDRESS TO LIST ONLY?                    
         BE    RDSLCTA                                                          
         CLI   8(R2),C'B'          ADD ADDRESS TO LIST AND GOTO IT?             
         BE    RDSLCTB                                                          
         CLI   8(R2),C'G'          GOTO ADDRESS?                                
         BE    RDSLCTG                                                          
         CLI   8(R2),C'X'          GOTO EXTENDED ADDRESS?                       
         BE    RDSLCTGX                                                         
         CLI   8(R2),C'P'          PARAMETER LIST?                              
         BE    RDSLCTP                                                          
         CLI   8(R2),C'S'          SAVE ADDRESS?                                
         BE    RDSLCTS                                                          
*                                                                               
         MVI   8(R2),C' '          OTHERWISE BLANK OUT THE SELECTION            
         OI    6(R2),X'80'                                                      
RDSLCTNX ZIC   R5,0(R2)            SKIP THE FIELD                               
         AR    R2,R5                                                            
         B     RDSLCTLP                                                         
         EJECT                                                                  
***********************************************************************         
* ADD THE ADDRESS USER SELECTED IN THE DUMP                           *         
***********************************************************************         
RDSLCTA  MVI   8(R2),C' '          BLANK OUT THE SELECTION                      
         OI    6(R2),X'80'                                                      
         LR    R4,R3               MAKE A COPY FOR LATER                        
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         ZIC   R5,0(R2)            SKIP TO THE COLUMN                           
         AR    R2,R5                                                            
         L     R5,SAVEAREA                                                      
         USING NDMPSAVD,R5                                                      
         LA    R6,NDMPCOLS         POINT TO THE FIRST DISPLACEMENT              
         SLL   R3,2                MULTIPLY OFFSET BY 4                         
         AR    R6,R3               POINT TO THE ADDR WE SELECTED                
         LA    R7,NDMPLIST         POINT TO LIST                                
         CLI   NDMPLCNT,0          NOTHING IN LIST?                             
         BE    STREADDR                                                         
         CLI   NDMPLCNT,24         OVERFLOW IN LIST?                            
         BE    LISTERR                                                          
         ZIC   R1,NDMPLCNT         FIND NUMBER OF ITEMS IN LIST                 
         MH    R1,=Y(L'NDMPLIST)                                                
         AR    R7,R1               POINT TO NEXT ITEM                           
STREADDR MVC   0(L'LSTADDR/2,R7),0(R6)    COPY ADDR AND CLEAR REMARK            
         XC    L'LSTADDR/2(L'LSTRMRK,R7),L'LSTADDR/2(R7)                        
         ZIC   R1,NDMPLCNT                                                      
         LA    R1,1(R1)            NEW NUMBER OF ITEMS IN LIST                  
         STC   R1,NDMPLCNT                                                      
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         LR    R3,R4               SAME COUNTER                                 
         B     RDSLCTNX            CAN ADD MULTIPLE, NOT GOTO MULTIPLE          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ADD THE ADDRESS USER SELECTED IN THE DUMP AND THEN GOTO IT          *         
***********************************************************************         
RDSLCTB  MVI   8(R2),C' '          BLANK OUT THE SELECTION                      
         OI    6(R2),X'80'                                                      
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         ZIC   R5,0(R2)            SKIP TO THE COLUMN                           
         AR    R2,R5                                                            
         L     R5,SAVEAREA                                                      
         USING NDMPSAVD,R5                                                      
         LA    R6,NDMPCOLS         POINT TO THE FIRST DISPLACEMENT              
         SLL   R3,2                MULTIPLY OFFSET BY 4                         
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         GOTO1 VHEXOUT,DMCB,1(R6),SRVP1,L'LSTADDR/2-1                           
         MVI   SRVP1H+5,L'LSTADDR-2                                             
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         LA    R7,NDMPLIST         POINT TO LIST                                
         CLI   NDMPLCNT,0          NOTHING IN LIST?                             
         BE    STORADDR                                                         
         CLI   NDMPLCNT,24         OVERFLOW IN LIST?                            
         BE    LISTERR                                                          
         ZIC   R1,NDMPLCNT         FIND NUMBER OF ITEMS IN LIST                 
         MH    R1,=Y(L'NDMPLIST)                                                
         AR    R7,R1               POINT TO NEXT ITEM                           
STORADDR MVC   0(L'LSTADDR/2,R7),0(R6)                                          
         XC    L'LSTADDR/2(L'LSTRMRK,R7),L'LSTADDR/2(R7)                        
         ZIC   R1,NDMPLCNT                                                      
         LA    R1,1(R1)            NEW NUMBER OF ITEM IN LIST                   
         STC   R1,NDMPLCNT                                                      
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         MVI   IND,0               START OVER WITH NEW PARAMETERS               
         MVI   REGADD,0                                                         
         MVI   NX,0                                                             
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS  CAN'T SET REGISTERS                      
         B     RDSLCTXT                                                         
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GOTO THE ADDRESS USER SELECTED IN THE DUMP                          *         
***********************************************************************         
RDSLCTG  MVI   8(R2),C' '          BLANK OUT THE SELECTION                      
         OI    6(R2),X'80'                                                      
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         ZIC   R5,0(R2)            SKIP TO THE COLUMN                           
         AR    R2,R5                                                            
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPCOLS         POINT TO THE FIRST DISPLACEMENT              
         DROP  R1                                                               
         SLL   R3,2                MULTIPLY OFFSET BY 4                         
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         GOTO1 VHEXOUT,DMCB,1(R6),SRVP1,L'LSTADDR/2-1                           
         MVI   SRVP1H+5,L'LSTADDR-2                                             
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         MVI   IND,0               START OVER WITH NEW PARAMETERS               
         MVI   REGADD,0                                                         
         MVI   NX,0                                                             
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS  CAN'T SET REGISTERS                      
         B     RDSLCTXT                                                         
         EJECT                                                                  
***********************************************************************         
* GOTO THE EXTENDED ADDRESS USER SELECTED IN THE DUMP                           
***********************************************************************         
RDSLCTGX MVI   8(R2),C' '          BLANK OUT THE SELECTION                      
         OI    6(R2),X'80'                                                      
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         ZIC   R5,0(R2)            SKIP TO THE COLUMN                           
         AR    R2,R5                                                            
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPCOLS         POINT TO THE FIRST DISPLACEMENT              
         DROP  R1                                                               
         SLL   R3,2                MULTIPLY OFFSET BY 4                         
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         GOTO1 VHEXOUT,DMCB,(R6),SRVP1,L'LSTADDR/2                              
         MVI   SRVP1H+5,L'LSTADDR                                               
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'0A'      VALID HEXIDECIMAL                            
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         OI    SRVP2H+6,X'80'                                                   
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         MVI   IND,0               START OVER WITH NEW PARAMETERS               
         MVI   REGADD,0                                                         
         MVI   NX,0                                                             
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS  CAN'T SET REGISTERS                      
         B     RDSLCTXT                                                         
         EJECT                                                                  
***********************************************************************         
* USE ADDRESS SELECTED IN THE DUMP AS THE BEGINNING OF OUR PARAMETER            
* LIST                                                                          
***********************************************************************         
RDSLCTP  MVI   8(R2),C' '          BLANK OUT THE SELECTION                      
         OI    6(R2),X'80'                                                      
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         ZIC   R5,0(R2)            SKIP TO THE COLUMN                           
         AR    R2,R5                                                            
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         LA    R6,NDMPCOLS         POINT TO THE FIRST DISPLACEMENT              
         DROP  R1                                                               
         SLL   R3,2                MULTIPLY OFFSET BY 4                         
         AR    R6,R3               POINT TO THE ONE WE SELECTED                 
         GOTO1 VHEXOUT,DMCB,1(R6),SRVP2,L'LSTADDR/2-1                           
         MVI   SRVP2H+5,L'LSTADDR-2                                             
         OI    SRVP2H+6,X'80'                                                   
         OI    SRVP2H+4,X'0A'      VALID HEXIDECIMAL                            
         MVC   SRVP1(5),=C'PARAM'                                               
         MVI   SRVP1H+5,5                                                       
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP1H+4,X'04'      VALID ALPHA                                  
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    SRVP3H+6,X'80'                                                   
         OI    STATFLAG,GOTOBIT    INDICATE THAT A GOTO WAS USED                
         MVI   IND,0               START OVER WITH NEW PARAMETERS               
         MVI   REGADD,0                                                         
         MVI   NX,0                                                             
         XC    DISP,DISP                                                        
         NI    STATFLAG,X'FF'-SETREGS   CAN'T SET REGISTERS                     
         B     RDSLCTXT                                                         
         EJECT                                                                  
***********************************************************************         
* SAVE THE ADDRESS USER SELECTED IN THE DUMP                          *         
***********************************************************************         
RDSLCTS  MVI   8(R2),C' '          BLANK OUT THE SELECTION                      
         OI    6(R2),X'80'                                                      
         ZIC   R5,0(R2)            SKIP TO THE COLUMN                           
         AR    R2,R5                                                            
         LR    R4,R3               MAKE A COPY FOR LATER                        
         BCTR  R3,0                OFFSET OF SELECT FIELD FROM FIRST            
         L     R5,SAVEAREA                                                      
         USING NDMPSAVD,R5                                                      
         LA    R6,NDMPADDR         POINT TO THE FIRST DISPLACEMENT              
         LA    R7,NDMPLIST         POINT TO LIST                                
         LR    R1,R3               SET UP FOR DIVIDE                            
         SR    R0,R0                                                            
         D     R0,=F'4'            R0=REMAINDER,R1=QUOTIENT                     
         SLL   R1,2                MULTIPLY BY 4 TO GET ADDR POSITION           
         AR    R6,R1               WHERE ADDRESS WAS STORED                     
         LR    R1,R0               GET REMAINDER                                
         L     R0,0(R6)            LOAD THE ADDRESS                             
         LR    R6,R0                                                            
         SLL   R1,2                MULTIPLY BY 4 TO GET ADDR POSITION           
         AR    R6,R1               ADDR WE SELECTED                             
         CLI   NDMPLCNT,0          NOTHING IN LIST?                             
         BE    SAVEADDR                                                         
         CLI   NDMPLCNT,24         OVERFLOW IN LIST?                            
         BE    LISTERR                                                          
         ZIC   R1,NDMPLCNT         FIND NUMBER OF ITEMS IN LIST                 
         MH    R1,=Y(L'NDMPLIST)                                                
         AR    R7,R1               POINT TO NEXT ITEM                           
*                                                                               
SAVEADDR ST    R6,0(R7)                                                         
         XC    L'LSTADDR/2(L'LSTRMRK,R7),L'LSTADDR/2(R7)                        
         ZIC   R1,NDMPLCNT                                                      
         LA    R1,1(R1)            NEW NUMBER OF ITEM IN LIST                   
         STC   R1,NDMPLCNT                                                      
         GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
         LR    R3,R4               SAME COUNTER                                 
         B     RDSLCTNX            CAN ADD MULTIPLE, NOT GOTO MULTIPLE          
         DROP  R5                                                               
* NONE SELECTED                                                                 
RDSLCTNN NI    STATFLAG,X'FF'-GOTOBIT  TURN OFF GOTO BIT                        
* EXIT STEP                                                                     
RDSLCTXT GOTO1 =A(CLSLCT),DMCB,(RC),(R2),RR=RELO    R2=A(CURRENT FLD)           
RDSLEXIT B     XIT                                                              
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CLEAR ALL SELECT FIELDS AFTER THE CURRENT FIELD POINTED BY R2       *         
***********************************************************************         
CLSLCT   DS    0H                                                               
         NMOD1 0,**CSEL**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BNZ   CLSLCTNX            YES                                          
CLSLCTLP CLI   0(R2),0             WENT BEYOND TWA?                             
         BE    CLSLCTEX            YES                                          
         MVI   8(R2),C' '                                                       
         OI    6(R2),X'80'         CLEAR THE SELECT FIELD                       
         ZIC   R5,0(R2)            SKIP TO PROTECTED FIELD                      
         AR    R2,R5                                                            
CLSLCTNX ZIC   R5,0(R2)            SKIP TO NEXT SELECT FIELD                    
         AR    R2,R5                                                            
         B     CLSLCTLP                                                         
CLSLCTEX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SECTION THAT PRINTS OUT THE TWA                                     *         
***********************************************************************         
TWACHK   NMOD1 0,**DTWA**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            A(TWA)                                       
         LA    R2,SRVTAGH          POINT TO WHERE SCREEN WILL GO                
         GOTO1 VCALLOV,DMCB,(X'FD',(R2)),0                                      
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         LA    RF,INA              FIND RESIDUAL L'REC FROM TWA START           
         A     RF,=A(RLEN)                                                      
         SR    RF,R5               RF = REMAING L'RECORD                        
         L     RE,ATIA             RE = A(TIA)                                  
         LR    R0,R5               R0 = A(TWA START)                            
         CH    RF,MAXTWAL          MOVE A MAXIMUM OF 1 TWA LENGTH               
         BNH   *+8                                                              
         LH    RF,MAXTWAL                                                       
         LR    R1,RF                                                            
         ST    RF,DUB              SAVE LENGTH TO BE MOVED                      
         MVCL  RE,R0                                                            
*                                                                               
         CLC   DUB+2(2),MAXTWAL                                                 
         BNL   TWACHK2             WHOLE TWA IN FIRST RECORD                    
         BRAS  RE,READDSK          GET REST OF TWA                              
         L     RE,ATIA                                                          
         A     RE,DUB              RE = A(TIA + L'FIRST MOVE)                   
         LH    RF,MAXTWAL                                                       
         S     RF,DUB              RF = L'REMAINDER OF TWA                      
         LA    R0,INA              R0 = A(START OF REMAINDER OF TWA)            
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE THE REMAINDER                           
*                                                                               
TWACHK2  XC    SRVPSW,SRVPSW       TRANSMIT START ADDR OF TWA                   
         MVC   SRVHPSW(6),=C'TWA==>'                                            
         GOTO1 VHEXOUT,DMCB,TWASTRT,SRVPSW,4                                    
*                                                                               
         L     R6,ATIA             START OF TWA                                 
         L     R7,DISP             DISP AS REQUESTED                            
         CH    R7,MAXTWAL                                                       
         BH    ADDRERR             DISP IS PAST TWAEND                          
         LTR   R7,R7                                                            
         BM    ADDRERR             DISP IS LT TWA START (IMPOSSIBLE?)           
         LA    RF,64(,R6)          RF = ADDRESS FIRST FIELD IN TWA              
         LR    R1,R6                                                            
         AH    R1,MAXTWAL          R1 = ADDRESS OF TWA END - 2                  
         SH    R1,=H'2'                                                         
         LA    R6,0(R7,R6)         R6 = ADDRESS AS REQUESTED                    
*        SCAN FOR FIRST TWA FIELD FOLLOWING DISP SPECIFIED                      
TWACHK2A CR    RF,R6               COMPARE SCAN TO REQUEST                      
         BE    TWACHK3             REQUEST IS A FIELD START                     
         BNL   TWACHK2B            SCAN IS NEXT FIELD AFTER REQUEST             
*        IF ANY FIELD ERRORS DURING SCAN. STOP AND ALLOW TWACHK4 TO             
*        FIND AND DISPLAY ERROR                                                 
         CLI   0(RF),79+8+8        IS FIELDLEN OK ?                             
         BH    TWACHK2B            NO - STOP SCAN HERE                          
         ZIC   R0,0(RF)            GET FIELDLEN                                 
         AR    R0,RF               R0 = NEXT FIELD                              
         CR    R0,R1               FIELD EXTEND BEYOND TWA ?                    
         BH    TWACHK2B            YES - STOP HERE                              
         CLI   0(RF),9             TOO SHORT ?                                  
         BL    TWACHK2B                                                         
*        THIS FIELD IS VALID AND NEXT FIELD IS ADDRESSABLE                      
         LR    RF,R0               CONTINUE SCAN WITH NEXT FIELD                
         B     TWACHK2A                                                         
*        MODIFY DISP TO REFLECT ACTUAL FIELD FOR DISPLAY                        
TWACHK2B LR    R6,RF               R6 = FIELD                                   
         S     RF,ATIA             RF = DISP OF FIELD                           
         ST    RF,DISP                                                          
TWACHK3  XC    LSTSCRP,LSTSCRP                                                  
         LA    R7,DMPL1H                                                        
         USING DMPLINED,R7                                                      
*                                                                               
TWACHK4  LA    R0,DMPLCH           6 SCREEN LINES MUST BE AVAILABLE             
         CR    R7,R0                                                            
         BH    TWACHKD             END THIS SCREEN                              
         CLI   0(R6),0                                                          
         BE    TWACHKC             END OF TWA                                   
         USING FLDHDRD,R6                                                       
*                                                                               
         LA    RF,TERR1            EXAMINE LENGTH FIELDS                        
         CLI   FLDLEN,79+8+8                                                    
         BH    TERROR              FIELD LENGTH TO BIG, GT 87 (79+8)            
         LA    RF,TERR9                                                         
         ZIC   R0,FLDLEN                                                        
         A     R0,DISP                                                          
         SH    R0,=H'2'                                                         
         CH    R0,MAXTWAL                                                       
         BH    TERROR              FIELD EXTENDS BEYOND VALID TWALEN            
         LA    RF,TERR2                                                         
         CLI   FLDLEN,9                                                         
         BL    TERROR                                                           
*                                                                               
         LA    RF,TERR5                                                         
         ZIC   RE,FLDLEN                                                        
         SH    RE,=H'8'                                                         
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         CLM   RE,1,FLDILEN                                                     
         BNL   TWACHK5                                                          
         TM    FLDATB,X'20'        PROTECTED AND                                
         BZ    TERROR                                                           
         TM    FLDIIND,X'0C'           SPECIAL FOR COLOR?                       
         BNO   TERROR              NO, THEN ERROR                               
*                                                                               
TWACHK5  LA    RF,TERR6                                                         
         NI    FLDOLEN,X'7F'                                                    
         CLM   RE,1,FLDOLEN                                                     
         BL    TERROR                                                           
*                                                                               
         CLI   FLDATB,X'FF'                                                     
         BE    TWACHK8             NOP FIELD, DISPLAY HDR AND DATA ONLY         
         LA    RF,TERR7            EXAMINE ATTRIBUTE BYTES                      
         TM    FLDATB,X'20'                                                     
         BO    *+12                PROTECTED                                    
         TM    FLDOIND,X'20'                                                    
         BZ    TWACHK6             UNPROTECTED                                  
*****    TM    FLDATB,X'01'                                                     
*****    BO    TERROR              PROTECTED & MODIFIED                         
*****    TM    FLDOIND,X'01'                                                    
*****    BO    TERROR              SAME, BUT TEMP MODIFIED                      
*                                                                               
TWACHK6  LA    RF,TERR3            EXAMINE SCREEN POSITION                      
         ZICM  RE,FLDADR,2                                                      
         BZ    TERROR              CANNOT USE POSITION 1                        
         LA    RF,TERR4                                                         
         CH    RE,=H'1919'                                                      
         BH    TERROR              CANNOT START ON LAST SCREEN POSN             
         LA    RF,TERR4A                                                        
         ZIC   R1,FLDLEN                                                        
         SH    R1,=H'8'                                                         
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         AR    RE,R1                                                            
         CH    RE,=H'1920'                                                      
         BH    TERROR              GOES OVER LAST SCREEN POSITION               
*                                                                               
         LA    RF,TERR3                                                         
         LH    RE,LSTSCRP                                                       
         CLM   RE,3,FLDADR                                                      
         BH    TERROR                                                           
*                                                                               
         LH    RE,FLDADR                                                        
         LR    R1,RE               CALCULATE FIRST VALID SCREEN ADDRESS         
         ZIC   RF,FLDLEN                                                        
         SH    RF,=H'8'                                                         
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         AR    RE,RF                                                            
         STH   RE,LSTSCRP                                                       
*                                                                               
         SR    R0,R0               DOES FIELD EXTEND BEYOND END                 
         D     R0,=F'80'           OF SCREEN LINE, UNPROT CAN'T USE             
         LA    RF,TERR8                                                         
         CH    R0,=H'80'                                                        
         BH    TERROR                                                           
         TM    FLDATB,X'20'                                                     
         BO    TWACHK8                                                          
*                                                                               
TWACHK8  CLI   TMODE,C'S'          DISPLAY HEADER DATA                          
         BE    TWACHKB                                                          
         BAS   RE,HEDOUT                                                        
         LA    R2,FLDDATA                                                       
         ZIC   R5,FLDLEN                                                        
         SH    R5,=H'8'                                                         
         CLI   FLDATB,X'FF'                                                     
         BE    TWACHKA             NOP FIELD, DISPLAY HDR AND DATA ONLY         
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         SH    R5,=H'8'                                                         
*                                                                               
TWACHKA  LA    R4,16               LIMIT PER LINE                               
         CR    R4,R5               LESS THAN FULL LINE?                         
         BNH   *+6                 NO                                           
         LR    R4,R5               YES, LOAD UP NUMBER                          
         GOTO1 VHEXOUT,DMCB,(R2),DMPLHEX,(R4)  DISPLAY HEX                      
         BCTR  R4,0                -1 FOR MVC                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DMPLALPH(0),0(R2)   COPY HEX ACROSS                              
         EX    R4,*+8                                                           
         B     *+10                                                             
         TR    DMPLALPH(0),TRTAB   TRANSLATE IT FOR US TO SEE                   
         LA    R7,DMPLNEXT         NEXT LINE                                    
         LA    R2,16(R2)                                                        
         SH    R5,=H'16'                                                        
         BP    TWACHKA                                                          
*                                                                               
TWACHKB  ZIC   RF,FLDLEN                                                        
         LR    RE,RF                                                            
         A     RF,DISP                                                          
         ST    RF,DISP                                                          
         LA    R6,0(RE,R6)                                                      
         B     TWACHK4                                                          
*                                                                               
TWACHKC  MVC   DMPLHEX(21),=C'END OF TWA - LENGTH ='                            
         L     RF,DISP                                                          
         AH    RF,=H'3'                                                         
         LA    RE,DMPLHEX+22                                                    
         EDIT  (RF),(4,(RE)),ALIGN=LEFT                                         
         LA    R7,DMPLNEXT                                                      
         XC    DISP,DISP                                                        
*                                                                               
TWACHKD  MVC   SRVMSG+7(L'TWAMSG),TWAMSG                                        
         MVC   0(3,R7),=X'000100'                                               
         EJECT                                                                  
TWAOUT   MVI   HALF,0                                                           
         ICM   R1,15,DISP          IS DISPLACEMENT -VE ?                        
         BNM   TWAOUT2                                                          
         MVI   HALF,C'-'                                                        
         LCR   R1,R1                                                            
         ST    R1,DISP                                                          
TWAOUT2  GOTO1 VHEXOUT,DMCB,DISP,DUB,4                                          
         XC    SRVP2,SRVP2                                                      
         XC    SRVP3,SRVP3                                                      
         LA    R1,DUB                                                           
         LA    R2,SRVP2                                                         
         LA    R3,8                                                             
         CLI   HALF,C'-'                                                        
         BNE   TWAOUT3                                                          
         MVI   0(R2),C'-'                                                       
         LA    R2,1(,R2)                                                        
TWAOUT3  CLI   0(R1),C'0'                                                       
         BNE   TWAOUT4                                                          
         LA    R1,1(,R1)                                                        
         BCT   R3,TWAOUT3                                                       
         MVC   SRVP2(4),DUB                                                     
         B     TWAOUT5                                                          
TWAOUT4  MVC   0(1,R2),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
         BCT   R3,TWAOUT4                                                       
TWAOUT5  NI    SRVP1H+6,X'FF'-X'40' UNSET CURSOR                                
         OI    SRVP3H+6,X'40'                                                   
         B     EXMOD                                                            
*                                                                               
TERROR   BAS   RE,HEDOUT                                                        
         MVC   DMPLSTRT(TMSGLNQ),0(RF)                                          
         LA    R7,DMPLNEXT                                                      
         B     TWACHKD                                                          
         EJECT                                                                  
HEDOUT   NTR1                                                                   
         L     R0,DISP                                                          
         A     R0,TWASTRT          CALCULATE CORE ADDR OF HEADER                
         ST    R0,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,DUB,4                                          
         MVC   DMPLSTRT,DUB+2       DISPLAY CORE ADDRESS                        
         GOTO1 (RF),(R1),DISP,DUB,4                                             
         MVC   DMPLHEX(4),DUB+4     DISPLAY HEADER TWA DISPLACEMENT             
         GOTO1 (RF),(R1),(R6),DMPLHEX+5,8                                       
         CLI   FLDATB,X'FF'        NOP FIELD ?                                  
         BNE   *+14                                                             
         MVC   DMPLHEX+22(9),=C'NOP FIELD'   YES                                
         B     HEDOUTX                                                          
         MVC   DMPLHEX+22(4),=C'ROW='                                           
         ZICM  R3,FLDADR,2                                                      
         SR    R2,R2               SCREEN ADDRESS BY ROW AND COLUMN             
         D     R2,=F'80'                                                        
         AH    R3,=H'1'                                                         
         AH    R2,=H'1'                                                         
         LA    R1,DMPLHEX+26                                                    
         EDIT  (R3),(2,(R1))                                                    
         OI    0(R1),X'F0'                                                      
         MVC   2(5,R1),=C',COL='                                                
         EDIT  (R2),(2,7(R1))                                                   
         OI    7(R1),X'F0'                                                      
*                                                                               
         LA    R1,9(R1)            MARK IF PROTECTED                            
         TM    FLDATB,X'20'                                                     
         BNZ   *+12                                                             
         TM    FLDOIND,X'20'                                                    
         BZ    *+14                                                             
         MVC   0(5,R1),=C',PROT'                                                
         LA    R1,5(R1)                                                         
*                                                                               
         TM    FLDATB,X'01'        MARK IF MODIFIED                             
         BNZ   *+12                                                             
         TM    FLDOIND,X'01'                                                    
         BZ    *+14                                                             
         MVC   0(4,R1),=C',MOD'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         TM    FLDOIND,X'40'       MARK IF CURSOR HERE                          
         BZ    *+14                                                             
         MVC   0(4,R1),=C',CUR'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
HEDOUTX  LA    R7,DMPLNEXT                                                      
         XIT1  REGS=(R7)                                                        
         EJECT                                                                  
*                                                                               
TWAMSG   DC    CL(L'COMPMSG)'TWA fields displayed - enter to page'              
*                                                                               
TERR1    DC    CL35'Field length gt then screen line'                           
TERR2    DC    CL35'Field length lt 9'                                          
TERR3    DC    CL35'Screen position overlaps last field'                        
TERR4    DC    CL35'Field starts beyond screen end'                             
TERR4A   DC    CL35'Field ends beyond screen end'                               
TERR5    DC    CL35'Input length is too big'                                    
TERR6    DC    CL35'Output length is too big'                                   
TERR7    DC    CL35'Trying to modify prot field'                                
TERR8    DC    CL35'Field goes over end of screen line'                         
TERR9    DC    CL35'Field extends beyond TWA max lnth'                          
TERRA    DC    CL35'Prot field cannot use 1st column'                           
*                                                                               
TMSGLNQ  EQU   L'TERR1                                                          
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SECTION THAT SHOWS SOFT STEREO FIELDS IN TBUFF                      *         
***********************************************************************         
SOFSHOW  NMOD1 0,*DSOFT**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            A(TWA)                                       
         LA    R2,SRVTAGH          POINT TO WHERE SCREEN WILL GO                
         GOTO1 VCALLOV,DMCB,(X'FD',(R2)),0                                      
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
SOFS02   LR    R6,R5               START OF TBUFF                               
         CLC   =C'VTRD',0(R6)                                                   
         BNE   SOFS04                                                           
         LA    R6,7(R6)                                                         
         CLI   0(R6),X'11'                                                      
         BNE   *+12                                                             
         LA    R6,3(R6)                                                         
         B     *-12                                                             
         LA    R6,3(R6)            3 BYTES AT FRONT                             
*                                                                               
SOFS04   LA    R7,DMPL1H                                                        
         USING DMPSOFTD,R7                                                      
*                                                                               
SOFS06   CLI   0(R6),X'03'         END OF TBUFF TERMINATOR                      
         BE    SOFS32                                                           
*                                                                               
         CLI   0(R6),X'11'         SBA TROUBLE                                  
         BNE   SOFS08                                                           
         LA    R6,3(R6)                                                         
         B     SOFS06                                                           
*                                                                               
SOFS08   LA    R0,DMPLFH           3 SCREEN LINES MUST BE AVAILABLE             
         CR    R7,R0                                                            
         BH    SOFS34              END THIS SCREEN                              
         USING SOFTFLD,R6                                                       
*                                                                               
         LR    RF,R6                                                            
         SR    RF,R5                                                            
         A     RF,START                                                         
         ST    RF,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL+1,DMPFADDR,3                                   
         MVC   FULL,0(R6)          COPY HEADER DETAILS                          
         GOTO1 VHEXOUT,DMCB,FULL,DMPFHHEX,4                                     
*                                                                               
HDR      USING SOFTFLD,FULL                                                     
         TR    FULL,DECTRAN                                                     
         CLI   HDR.SOFTID,15       PROTECTED?                                   
         BH    *+10                NO                                           
         MVC   DMPFHHEX+6(2),=CL2'  '                                           
*                                                                               
         LA    RF,SFFROW       *** SEE IF FIELD IS A ROW CHANGE                 
SOFS10   CLI   0(RF),255           TRY TO MATCH FIELD                           
         BE    SOFS14                                                           
         CLC   SOFTID,0(RF)                                                     
         BE    SOFS12                                                           
         LA    RF,L'SFFROW(RF)                                                  
         B     SOFS10                                                           
*                                                                               
SOFS12   MVC   DMPFROWH,SNROW                                                   
         EDIT  (B1,1(RF)),(3,DMPFROW),ALIGN=LEFT,ZERO=NOBLANK                   
         LA    R6,1(R6)            NEXT FIELD                                   
         LA    R7,DMPFNEXT                                                      
         B     SOFS06                                                           
*                                                                               
SOFS14   LA    RF,SFFID        *** SEE IF FIELD IS A FIELD                      
*                                                                               
SOFS16   CLI   0(RF),255           TRY TO MATCH FIELD                           
         BE    SOFS26                                                           
         CLC   SOFTID,0(RF)                                                     
         BE    SOFS18                                                           
         LA    RF,L'SFFID(RF)                                                   
         B     SOFS16                                                           
*                                                                               
SOFS18   MVC   DMPFHFLD,1(RF)      SET FIELD TYPE                               
*                                                                               
         LA    R3,DMPFHCL                                                       
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
*                                                                               
         MVC   0(L'SFCOL,R3),SFCOL    'COLUMN='                                 
         LA    R3,L'SFCOL(R3)                                                   
         XR    R0,R0               EDIT OUT COLUMN                              
         IC    R0,HDR.SOFTCOL                                                   
         EDIT  (R0),(3,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         MVC   1(L'SFLEN,R3),SFLEN    'DATA LENGTH='                            
         LA    R3,L'SFLEN+1(R3)                                                 
         XR    R0,R0               EDIT OUT DATA LENGTH                         
         IC    R0,HDR.SOFTDLEN                                                  
         EDIT  (R0),(3,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R3,R0                                                            
         CLI   HDR.SOFTID,15       PROTECTED?                                   
         BNH   SOFS20              YES                                          
*                                                                               
         MVI   0(R3),C','                                                       
         MVC   1(L'SFDLEN,R3),SFDLEN  'FLD LEN='                                
         LA    R3,L'SFDLEN+1(R3)                                                
         XR    R0,R0               EDIT OUT FIELD LENGTH                        
         IC    R0,HDR.SOFTFLEN                                                  
         EDIT  (R0),(3,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
*                                                                               
SOFS20   LA    R7,DMPFNEXT         NEXT LINE                                    
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,HDR.SOFTDLEN   DATA LENGTH                                  
         BNZ   SOFS21              NOT ZERO DATA LENGTH                         
*                                                                               
         MVC   DMPFINF,0(R6)       MOVE OUT FIELD CHARACTERS                    
         CLI   HDR.SOFTID,15       PROTECTED?                                   
         BH    *+8                 NO                                           
         MVI   DMPFINF+L'DMPFINF-1,C' '                                         
*                                                                               
         MVC   DMPFDTA(L'SFZERO),SFZERO                                         
         XR    R0,R0                                                            
         B     SOFS25                                                           
*                                                                               
SOFS21   LR    RF,R0               LOOK FOR SBA WITHIN DATA                     
*                                                                               
         LA    RE,SOFTUDTA                                                      
         CLI   HDR.SOFTID,15       PROTECTED?                                   
         BH    *+8                 NO                                           
         LA    RE,SOFTPDTA                                                      
*                                                                               
SOFS22   CLI   0(RE),X'11'         SBA WITHIN DATA FIELD?                       
         BNE   SOFS24                                                           
         LA    RF,3(RF)            NEW LENGTH TO NEXT FIELD                     
         LA    R1,3(RE)                                                         
         LR    R3,R0                                                            
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,RE),0(R1)       COPY DATA OVER SBA                           
*                                                                               
SOFS24   LA    RE,1(RE)                                                         
         BCT   R0,SOFS22                                                        
         LR    R0,RF               SAVE NEW DATA LENGTH                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,HDR.SOFTDLEN   DATA LENGTH                                  
         BCTR  RF,0                                                             
*                                                                               
         MVC   DMPFINF,0(R6)                                                    
         CLI   HDR.SOFTID,15       PROTECTED?                                   
         BH    *+8                 NO                                           
         MVI   DMPFINF+L'DMPFINF-1,C' '                                         
*                                                                               
         LA    RE,SOFTUDTA                                                      
         CLI   HDR.SOFTID,15       PROTECTED?                                   
         BH    *+8                 NO                                           
         LA    RE,SOFTPDTA                                                      
         MVC   DMPFDTA(L'SFDATA),SFDATA                                         
         LA    R1,DMPFDTA+L'SFDATA                                              
*                                                                               
         CLM   RF,1,=AL1(50)       TEMP                                         
         BNH   *+8                                                              
         LA    RF,50                                                            
         EX    RF,*+4                                                           
         MVC   0(0,R1),0(RE)                                                    
*                                                                               
SOFS25   LA    RF,3                                                             
         CLI   HDR.SOFTID,15       PROTECTED?                                   
         BNH   *+8                 YES                                          
         LA    RF,4                                                             
         AR    R0,RF               ADD HEADER LENGTH                            
         AR    R6,R0                                                            
         LA    R7,DMPFNEXT                                                      
         B     SOFS06                                                           
*                                                                               
SOFS26   CLI   SOFTID,X'E5'        CURSOR EXCEPTION?                            
         BNE   SOFS28                                                           
         MVC   DMPFCURH,SFCUR                                                   
         XR    R0,R0               EDIT OUT CURSOR DISPLACEMENT                 
         IC    R0,HDR.SOFTCOL                                                   
         EDIT  (R0),(3,DMPFCUR),ALIGN=LEFT,ZERO=NOBLANK                         
         LA    R6,2(R6)                                                         
         LA    R7,DMPFNEXT         NEXT ROW                                     
         B     SOFS06                                                           
*                                                                               
SOFS28   CLI   SOFTID,X'A2'        STEREO FIELD?                                
         BNE   SOFS30                                                           
         MVC   DMPFSSF,SFSSF       'SET STEREO FIELD ='                         
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R0,1,HDR.SOFTCOL                                                 
         SLL   R0,6                                                             
         ICM   R1,1,HDR.SOFTDLEN                                                
         AR    R0,R1                                                            
         EDIT  (R0),(3,DMPFSSF),ALIGN=LEFT,ZERO=NOBLANK                         
         LA    R6,3(R6)                                                         
         LA    R7,DMPFNEXT         NEXT ROW                                     
         B     SOFS06                                                           
*                                                                               
SOFS30   MVC   DMPFCURH(L'SFINV),SFINV                                          
         LA    R7,DMPFNEXT                                                      
         MVC   DMPFCURH(20),0(R6)                                               
         LA    R7,DMPFNEXT                                                      
         GOTO1 VHEXOUT,DMCB,0(R6),DMPFCURH,20,0                                 
         LA    R7,DMPFNEXT                                                      
         LR    R6,R5                                                            
         XC    DISP,DISP                                                        
         MVC   SRVMSG+7(L'SOFSERR1),SOFSERR1                                    
         B     SOFS36                                                           
*                                                                               
SOFS32   MVC   DMPFCURH(L'SFEOT),SFEOT                                          
         LA    R7,DMPFNEXT                                                      
         LR    R6,R5                                                            
         XC    DISP,DISP                                                        
         MVC   SRVMSG+7(L'SOFSMSG1),SOFSMSG1                                    
         B     SOFS36                                                           
*                                                                               
SOFS34   MVC   SRVMSG+7(L'SOFSMSG),SOFSMSG                                      
SOFS36   MVC   0(3,R7),=X'000100'                                               
         MVI   HALF,0                                                           
         ICM   R1,15,DISP                                                       
         SR    R6,R5                                                            
         AR    R1,R6                                                            
         ST    R1,DISP                                                          
         XC    SRVP2,SRVP2                                                      
*                                                                               
SOFS38   GOTO1 VHEXOUT,DMCB,DISP,DUB,4                                          
         LA    R1,DUB                                                           
         LA    R2,SRVP2                                                         
         LA    R3,8                                                             
SOFS40   CLI   0(R1),C'0'                                                       
         BNE   SOFS42                                                           
         LA    R1,1(,R1)                                                        
         BCT   R3,SOFS40                                                        
         MVC   SRVP2(4),DUB                                                     
         B     SOFS44                                                           
SOFS42   MVC   0(1,R2),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
         BCT   R3,SOFS42                                                        
SOFS44   NI    SRVP1H+6,X'FF'-X'40' UNSET CURSOR                                
         NI    SRVP2H+6,X'FF'-X'40' UNSET CURSOR                                
         MVC   SRVP3(6),=C'STEREO'                                              
         OI    SRVP3H+(FHOI-FHD),FHOITR                                         
         OI    DMPL1H+(FHOI-FHD),FHOICU                                         
         B     EXMOD                                                            
         EJECT                                                                  
*                                                                               
SOFSMSG  DC    CL(L'COMPMSG)'Stereo Translation - Enter to page'                
SOFSMSG1 DC    CL(L'COMPMSG)'Stereo Translation - Enter for first'              
SOFSERR1 DC    CL(L'COMPMSG)'Stereo Error - Enter for first'                    
*                                                                               
SFFID    DS    0CL19                                                            
         DC    X'D1',CL18'Prot,Zero'               10                           
         DC    X'D2',CL18'Prot,Normal'             11                           
         DC    X'D3',CL18'Prot,High'               12                           
         DC    X'D4',CL18'Prot,Zero,Mod'           13                           
         DC    X'D5',CL18'Prot,Normal,Mod'         14                           
         DC    X'D6',CL18'Prot,High,Mod'           15                           
*                                                                               
         DC    X'D7',CL18'Unprot,Zero'             16                           
         DC    X'D8',CL18'Unprot,Normal'           17                           
         DC    X'D9',CL18'Unprot,High'             18                           
         DC    X'E2',CL18'Unprot,Zero,Mod'         19                           
         DC    X'E3',CL18'Unprot,Normal,Mod'       20                           
         DC    X'E4',CL18'Unprot,High,Mod'         21                           
         DC    X'FF'                                                            
*                                                                               
SFFROW   DS    0CL2                ROWS 1-24                                    
         DC    X'F4',AL1(01)                                                    
         DC    X'F5',AL1(02)                                                    
         DC    X'F6',AL1(03)                                                    
         DC    X'F7',AL1(04)                                                    
         DC    X'F8',AL1(05)                                                    
         DC    X'F9',AL1(06)                                                    
         DC    X'81',AL1(07)                                                    
         DC    X'82',AL1(08)                                                    
         DC    X'83',AL1(09)                                                    
         DC    X'84',AL1(10)                                                    
         DC    X'85',AL1(11)                                                    
         DC    X'86',AL1(12)                                                    
         DC    X'87',AL1(13)                                                    
         DC    X'88',AL1(14)                                                    
         DC    X'89',AL1(15)                                                    
         DC    X'91',AL1(16)                                                    
         DC    X'92',AL1(17)                                                    
         DC    X'93',AL1(18)                                                    
         DC    X'94',AL1(19)                                                    
         DC    X'95',AL1(20)                                                    
         DC    X'96',AL1(21)                                                    
         DC    X'97',AL1(22)                                                    
         DC    X'98',AL1(23)                                                    
         DC    X'99',AL1(24)                                                    
         DC    X'FF'                                                            
         SPACE 2                                                                
SNROW    DC    C'Set new row='                                                  
SFCOL    DC    C'Column='                                                       
SFLEN    DC    C'Data len='                                                     
SFDLEN   DC    C'Fld len='                                                      
SFCUR    DC    C'Cursor exception,displacement='                                
SFSSF    DC    C'Set stereo field number='                                      
SFDATA   DC    C'Data='                                                         
SFEOT    DC    C'End of buffer found'                                           
SFINV    DC    C'**ERROR** Cannot resolve data'                                 
SFZERO   DC    C'Field has zero length'                                         
         LTORG                                                                  
*                                                                               
*        DECIMAL TO EBCDIC TRANSLATION TABLE FOR STEREO DATA BUFFER             
*                                                                               
DECTRAN  DC    XL16'00000000000000000000000000000000'  00-0F                    
         DC    XL16'00000000000000000000000000000000'  10-1F                    
         DC    XL16'00000000000000000000000000000000'  20-2F                    
         DC    XL16'00000000000000000000000000000000'  30-3F                    
         DC    XL16'00000000000000000000004041424300'  40-4F                    
         DC    XL16'45000000000000000000464748494A00'  50-5F                    
         DC    XL16'4C4D0000000000000000004F50515253'  60-6F                    
         DC    XL16'000000000000000000545556573F444B'  70-7F                    
         DC    XL16'0025262728292A2B2C2D000000000000'  80-8F                    
         DC    XL16'002E2F30313233343536000000000000'  90-9F                    
         DC    XL16'00003738393A3B3C3D3E000000000000'  A0-AF                    
         DC    XL16'00000000000000000000000000000000'  B0-BF                    
         DC    XL16'4E010203040506070809000000000000'  C0-CF                    
         DC    XL16'000A0B0C0D0E0F101112000000000000'  D0-DF                    
         DC    XL16'0000131415161718191A000000000000'  E0-EF                    
         DC    XL16'1B1C1D1E1F2021222324000000000000'  F0-FF                    
*                                                                               
SOFTFLD  DSECT                                                                  
SOFTID   DS    X                   FIELD ID                                     
SOFTCOL  DS    X                   COLUMN NUMBER                                
SOFTDLEN DS    X                   DATA LENGTH                                  
SOFTPDTA DS    0X                  PROTECTED DATA STARTS HERE                   
SOFTFLEN DS    X                   FIELD LENGTH (UNPROTECTED)                   
SOFTUDTA DS    X                   UNPROTECTED DATA STARTS HERE                 
*                                                                               
NDUMP    CSECT                                                                  
         DROP  HDR                                                              
         EJECT                                                                  
***********************************************************************         
* SECTION THAT SHOWS HOW A PORTION OF THE TWA LOOKS LIKE              *         
***********************************************************************         
TWASHOW  DS    0H                                                               
         NMOD1 0,**TSHW**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            A(TWA)                                       
         LA    R2,SRVTAGH          POINT TO WHERE SCREEN WILL GO                
         GOTO1 VCALLOV,DMCB,(X'FD',(R2)),0                                      
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         LA    RF,INA              FIND RESIDUAL L'REC FROM TWA START           
         A     RF,=A(RLEN)                                                      
         SR    RF,R5               RF = REMAING L'RECORD                        
         L     RE,ATIA             RE = A(TIA)                                  
         LR    R0,R5               R0 = A(TWA START)                            
         CH    RF,MAXTWAL          MOVE A MAXIMUM OF 1 TWA LENGTH               
         BNH   *+8                                                              
         LH    RF,MAXTWAL                                                       
         LR    R1,RF                                                            
         ST    RF,DUB              SAVE LENGTH TO BE MOVED                      
         MVCL  RE,R0                                                            
*                                                                               
         CLC   DUB+2(2),MAXTWAL                                                 
         BNL   SHWTWAST            WHOLE TWA IN FIRST RECORD                    
         BRAS  RE,READDSK          GET REST OF TWA                              
         L     RE,ATIA                                                          
         A     RE,DUB              RE = A(TIA + L'FIRST MOVE)                   
         LH    RF,MAXTWAL                                                       
         S     RF,DUB              RF = L'REMAINDER OF TWA                      
         LA    R0,INA              R0 = A(START OF REMAINDER OF TWA)            
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE THE REMAINDER                           
*                                                                               
SHWTWAST XC    SRVPSW,SRVPSW       TRANSMIT START ADDR OF TWA                   
         MVC   SRVHPSW(6),=C'TWA==>'                                            
         GOTO1 VHEXOUT,DMCB,TWASTRT,SRVPSW,4                                    
*                                                                               
         L     R6,ATIA             START OF TWA                                 
         LA    R6,64(,R6)          R6 = ADDRESS FIRST FIELD IN TWA              
         MVC   DMPL1,=80C'*'       BORDER FROM INFORMATION AND SCREEN           
         XC    DISP,DISP                                                        
         XC    LSTSCRP,LSTSCRP                                                  
STWALP   DS    0H                                                               
         CLI   0(R6),0             END OF TWA?                                  
         BNE   STWA0               NO                                           
         CLI   IND,C'U'            UPPER TWA?                                   
         BNE   STWALWR             NO, LOWER                                    
STWAUPR  MVC   SRVMSG+7(L'UTWAMSG),UTWAMSG   YES, NO NEED FOR LOWER             
         OI    SRVMSGH+6,X'80'                                                  
         B     EXITSTWA                                                         
STWALWR  MVC   SRVMSG+7(L'VTWAMSG),VTWAMSG                                      
         OI    SRVMSGH+6,X'80'                                                  
         MVC   SRVP1(3),=C'TTU'    SWITCH BETWEEN UPPER AND LOWER               
         OI    SRVP1H+6,X'80'                                                   
         B     EXITSTWA                                                         
STWA0    LA    RF,STWAERR2                                                      
         CLI   0(R6),9                                                          
         BL    STWAERR                                                          
         CLI   1(R6),X'FF'                                                      
         BE    SHWTWANX            NOP FIELD, CAN'T SEE IT                      
         LA    RF,STWAERR1                                                      
         CLI   0(R6),79+8+8                                                     
         BH    STWAERR                                                          
*                                                                               
         LA    RF,STWAERR5                                                      
         ZIC   RE,0(R6)                                                         
         SH    RE,=H'8'                                                         
         TM    1(R6),X'02'         ANY EXTENDED HEADER?                         
         BZ    *+8                                                              
         SH    RE,=H'8'            REMOVE LENGTH OF EXTENDED HEADER             
         CLM   RE,1,5(R6)                                                       
         BNL   STWA0A                                                           
         TM    1(R6),X'20'         PROTECTED AND                                
         BZ    STWAERR                                                          
         TM    4(R6),X'0C'             SPECIAL CODE FOR COLOR?                  
         BNO   STWAERR             NO, THEN ERROR                               
*                                                                               
STWA0A   LA    RF,STWAERR6                                                      
         NI    7(R6),X'FF'-X'80'                                                
         CLM   RE,1,7(R6)                                                       
         BL    STWAERR                                                          
         LA    RF,STWAERR3                                                      
         ZICM  R5,2(R6),2          LOAD DATA SCREEN ADDRESS                     
         BZ    STWAERR                                                          
         LA    RF,STWAERR4                                                      
         CH    R5,=H'1919'                                                      
         BH    STWAERR                                                          
         LA    RF,STWAER4A                                                      
         LR    R1,R5                                                            
         AR    R1,RE                                                            
         CH    R1,=H'1920'                                                      
         BH    STWAERR                                                          
         LA    RF,STWAERR7                                                      
         TM    1(R6),X'20'                                                      
         BO    *+12                                                             
         TM    6(R6),X'20'                                                      
         BZ    STWA1                                                            
         TM    1(R6),X'01'                                                      
         BO    STWAERR                                                          
         TM    6(R6),X'01'                                                      
         BO    STWAERR                                                          
STWA1    LA    RF,STWAERR3                                                      
         LH    R1,LSTSCRP                                                       
         CLM   R1,3,2(R6)                                                       
         BH    STWAERR                                                          
         LR    R1,R5                                                            
         AR    R1,RE                                                            
         STH   R1,LSTSCRP                                                       
         LR    R1,R5                                                            
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LA    RF,STWAERR8                                                      
         CH    R0,=H'80'                                                        
         BH    STWAERR                                                          
         LA    RF,STWAERR9                                                      
         ZIC   R1,0(R6)                                                         
         A     R1,DISP                                                          
         SH    R1,=H'2'                                                         
         CH    R1,MAXTWAL                                                       
         BH    STWAERR                                                          
         CLI   IND,C'U'            UPPER TWA?                                   
         BNE   SHWVTWA                                                          
         CH    R5,=H'1280'         ROW 17 COL 1?                                
         BL    SHWTWA                                                           
         MVC   SRVP1(3),=C'TTL'    SWITCH BETWEEN UPPER AND LOWER               
         OI    SRVP1H+6,X'80'                                                   
         B     STWAUPR             UPPER PORTION MESSAGE                        
SHWVTWA  CH    R5,=H'640'          ROW  9 COL 1?                                
         BL    SHWTWANX            NO, GET THE NEXT FIELD                       
SHWTWA   LA    R3,80                                                            
         SR    R4,R4                                                            
         DR    R4,R3               R5=ROW OFFSET, R4=COL OFFSET                 
         BCTR  R4,0                -1 FOR OFFSET INTO OUR TWA DATA              
         CLI   IND,C'V'            LOWER TWA?                                   
         BNE   *+8                                                              
         SH    R5,=H'8'            LINE 9 IS FIRST LINE TO DISPLAY              
         LA    R3,7(R3)            SKIP OVER HEADERS IN OUR TWA (79+8)          
         MR    R2,R5               R3 = LINE TO DISPLAY ON                      
         AR    R3,R4               R3 = COL ON THE LINE TO START                
         LA    R1,DMPL2H           SKIP HEADER OF ASTERIX                       
         LA    R2,0(R1,R3)         OUR ADDRESS TO START DISPLAY                 
         TM    1(R6),X'0C'         LOW INTENSITY                                
         BO    SHWTWANX               IS NOT SHOWN                              
         BCTR  RE,0                -1 FOR MVC                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R6)       COPY THE TEXT                                
         XC    DMCB(64),DMCB                                                    
         MVI   DMCB,C'T'           TRANSLATE WHOLE FIELD                        
         MVI   DMCB+1,C'U'                                                      
         TM    1(R6),X'40'         LOWER CASE?                                  
         BZ    *+8                 NO                                           
         MVI   DMCB+1,C'L'                                                      
         L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         MVC   DMCB+2(1),NDMPSYSN  GET SYSTEM NUMBER OF PROGRAM                 
         DROP  R1                        THAT DUMP                              
         MVI   DMCB+3,0            ENGLISH, PLEASE                              
         LA    RF,8(R2)            SET ADDRESS AND LENGTH OF DATA               
         ST    RF,DMCB+4                                                        
         LA    RE,1(RE)                                                         
         STC   RE,DMCB+4                                                        
         LA    R1,DMCB                                                          
         L     RF,VDICTATE                                                      
         BASR  RE,RF                                                            
SHWTWANX ZIC   RE,0(R6)                                                         
         LR    R1,RE                                                            
         A     RE,DISP                                                          
         ST    RE,DISP                                                          
         AR    R6,R1                                                            
         B     STWALP                                                           
*                                                                               
EXITSTWA LA    R2,DMPL1H           POINT TO FIRST LINE                          
         SR    R1,R1               CLEAR HOB FOR IC                             
XITSHWLP OI    6(R2),X'80'         TRANSMIT THE LINES                           
         OI    1(R2),X'08'         HIGH INTENSITY                               
         IC    R1,0(R2)            LENGTH OF FIELD                              
         AR    R2,R1               POINT TO NEXT FIELD                          
         CLI   0(R2),0             END OF OUR TWA?                              
         BNE   XITSHWLP            NO                                           
         NI    SRVP1H+6,X'FF'-X'40' UNSET CURSOR                                
         OI    SRVP2H+6,X'40'                                                   
         B     EXMOD                                                            
         SPACE 2                                                                
STWAERR  MVC   SRVMSG+L'ERRHDR(L'STWAERR1),0(RF)                                
         MVC   SRVMSG(L'ERRHDR),ERRHDR                                          
         MVI   SRVMSG+8,C'('                                                    
         MVC   SRVMSG+9(L'SYSNAME),SYSNAME                                      
         LA    RF,SRVMSG+L'SYSNAME+9                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         MVC   SRVMSG+2(1),SYSCH                                                
         OI    SRVMSGH+6,X'80'                                                  
         B     EXITSTWA                                                         
         SPACE 2                                                                
UTWAMSG  DC    CL35'Upper portion of the TWA displayed'                         
VTWAMSG  DC    CL35'Lower portion of the TWA displayed'                         
STWAERR1 DC    CL35'Field length gt than screen line'                           
STWAERR2 DC    CL35'Field length lt 9'                                          
STWAERR3 DC    CL35'Screen position overlaps last field'                        
STWAERR4 DC    CL35'Field starts beyond screen end'                             
STWAER4A DC    CL35'Field ends beyond screen end'                               
STWAERR5 DC    CL35'Input length is too big'                                    
STWAERR6 DC    CL35'Output length is too big'                                   
STWAERR7 DC    CL35'Trying to modify prot field'                                
STWAERR8 DC    CL35'Field goes over end of screen line'                         
STWAERR9 DC    CL35'Field extends beyond TWA max lnth'                          
STWAERRA DC    CL35'Prot field cannot use 1st column'                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SHOW WHOAMI TYPE INFORMATION                             *         
***********************************************************************         
         SPACE 1                                                                
WHOAMI   NMOD1 WHOWORKL,NWHOMI,RR=RE,CLEAR=YES                                  
         LR    R7,RC                                                            
         USING WHOWRKD,R7                                                       
         ST    RE,WHORELO                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            TWA                                          
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'FB',SRVCLRH),0                                   
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         LR    R2,R5                                                            
         USING UTLD,R2                                                          
         OC    TPRNT,TPRNT         PRINTER?                                     
         BNZ   WHO24               YES                                          
*                                                                               
WHO02    OC    TUSER,TUSER         CONNECTED?                                   
         BZ    WHOO10                                                           
*                                                                               
         XC    KEY,KEY             BUILD KEY OF ID REC                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TUSER                                                  
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,KEY                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         XR    RF,RF                                                            
WHO04    CLI   0(R6),0             FIND DESCRIPTION ELEMENT                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),2                                                          
         BE    WHO06                                                            
         ICM   RF,1,1(R6)                                                       
         BZ    *+8                                                              
         BXH   R6,RF,WHO04                                                      
         DC    H'0'                                                             
*                                                                               
WHO06    MVC   SRVCID,2(R6)                                                     
*                                                                               
         OC    TSVCREQ,TSVCREQ     SERVICE REQUEST?                             
         BZ    WHOO07              NO                                           
         MVC   SRVCSS,=CL7'SERVICE'                                             
         MVC   SRVCPG(4),=C'Pgm='                                               
         GOTO1 VHEXOUT,DMCB,TSVCREQ+1,SRVCPG+4,1,=C'TOG'                        
         B     WHOO10                                                           
*                                                                               
WHOO07   CLI   TSYS,0              CONNECTED SYSTEM?                            
         BE    WHOO10                                                           
*                                                                               
         LA    R6,SYSLST                                                        
         USING SYSLSTD,R6                                                       
         LH    RE,0(R6)                                                         
         ICM   RF,15,2(R6)                                                      
         A     RF,WHORELO                                                       
         LA    R6,6(R6)                                                         
         CLC   TOVSYS,SYSLNUM                                                   
         BE    *+10                                                             
         BXLE  R6,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   SRVCSS,SYSLNAME     MOVE IN SYSTEM NAME                          
*                                                                               
         L     R6,VSELIST                                                       
         LH    RE,0(R6)                                                         
         ICM   RF,15,2(R6)                                                      
         LA    R6,6(R6)                                                         
         USING SELISTD,R6                                                       
         CLC   TSYS,SESYS                                                       
         BE    *+10                                                             
         BXLE  R6,RE,*-10                                                       
         DC    H'0'                                                             
         ST    R6,ASYS             SAVE A(SELIST ENTRY)                         
*                                                                               
         L     R6,SEPGMS           PROGRAM                                      
         LH    RE,0(R6)                                                         
         ICM   RF,15,2(R6)                                                      
         LA    R6,6(R6)                                                         
         USING PGMLSTD,R6                                                       
         CLC   PGMNUM,TPRG                                                      
         BE    WHO08                                                            
         BXLE  R6,RE,*-10                                                       
         MVC   SRVCPG(4),=C'Pgm='                                               
         GOTO1 VHEXOUT,DMCB,TPRG,SRVCPG+4,1,=C'TOG'                             
         B     WHOO10                                                           
*                                                                               
WHO08    MVC   SRVCPG,PGMNAME      CONNECTED PROGRAM                            
*                                                                               
WHOO10   L     R6,VSSB                                                          
         L     R6,SSBACTRY-SSBD(R6)                                             
         LH    RE,0(R6)                                                         
         ICM   RF,15,2(R6)                                                      
         LA    R6,6(R6)                                                         
         USING CTRYTABD,R6                                                      
         CLC   CTRYCODE,TCTRY                                                   
         BE    WHOO12                                                           
         BXLE  R6,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
WHOO12   MVC   SRVCTY,CTRYNAMN                                                  
*                                                                               
         MVC   DUB(1),TLANG                                                     
         NI    DUB,X'0F'                                                        
         L     R6,VSSB                                                          
         L     R6,SSBALANG-SSBD(R6)                                             
         LH    RE,0(R6)                                                         
         ICM   RF,15,2(R6)                                                      
         LA    R6,6(R6)                                                         
         USING LANGTABD,R6                                                      
         CLC   LANGCODE,DUB                                                     
         BE    WHOO16                                                           
         BXLE  R6,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
WHOO16   MVC   SRVLNG,LANGFULN                                                  
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         EDIT  SSBSYSID,(2,SRVFACN+7),ALIGN=LEFT                                
         MVC   SRVFAC,SSBVTID                                                   
         CLC   SSBVTID,=C'NEW     '                                             
         BNE   *+10                                                             
         MVC   SRVFAC,=C'FACNEW  '                                              
         DROP  RE                                                               
*                                                                               
WHO10    OC    TUSER,TUSER         CONNECTED TO A USERID?                       
         BZ    WHO11                                                            
         EDIT  (B2,TUSER),(4,SRVIDN),ALIGN=LEFT                                 
         LA    R1,SRVIDN                                                        
         AR    R1,R0                                                            
         MVI   0(R1),C'/'                                                       
         MVC   1(2,R1),TAGY                                                     
*                                                                               
WHO11    CLI   TSYS,0              SE NUMBER                                    
         BE    WHO12                                                            
         EDIT  (B1,TSYS),(3,SRVSEN),ALIGN=LEFT                                  
         LA    R1,SRVSEN           BUMP TO POSTION AFTER SE NUMBER              
         AR    R1,R0                                                            
         MVI   0(R1),C'/'                                                       
         L     RF,ASYS                                                          
         MVC   1(7,R1),SENAME-SELISTD(RF)                                       
*                                                                               
WHO12    TM    TTEST,X'23'         TEST LEVEL                                   
         BZ    WHO14                                                            
         MVC   DUB(1),TTEST                                                     
         NI    DUB,X'03'                                                        
         OI    DUB,X'C0'                                                        
         MVC   SRVTSTL(1),DUB                                                   
         LA    R1,SRVTSTL+1                                                     
*                                                                               
         CLI   DUB,X'C0'                                                        
         BNE   *+14                                                             
         MVC   SRVTSTL(3),=C'Yes'                                               
         LA    R1,SRVTSTL+3                                                     
*                                                                               
         TM    TTEST,X'20'                                                      
         BZ    *+10                                                             
         MVC   0(8,R1),=C',cil=yes'                                             
*                                                                               
WHO14    CLI   TSYS,0              PRGM AUTH                                    
         BE    WHO16                                                            
         GOTO1 VHEXOUT,DMCB,TAUTH,SRVAUTH,2                                     
*                                                                               
WHO16    TM    TTEST,X'10'         TEST ID                                      
         BZ    WHO18                                                            
         GOTO1 VHEXOUT,DMCB,TACCS,SRVAACC,4,=C'TOG'                             
         ICM   RF,15,TACCS                                                      
         USING TSTTABD,RF                                                       
         MVC   SRVTSTI,TSTACCS                                                  
         B     WHO19                                                            
         DROP  RF                                                               
*                                                                               
WHO18    CLI   TSYS,0              ACCESS CODE                                  
         BE    WHO19                                                            
         OC    TACCS,TACCS                                                      
         BZ    WHO19                                                            
         MVC   SRVAACT,=CL11'Access Code'                                       
         MVC   SRVTSTT,=CL11'           '                                       
         MVC   SRVAACC(4),TACCS                                                 
*&&US                                                                           
         ICM   R5,15,ASYS          A(SELIST ENTRY)                              
         BZ    WHO19                                                            
         USING SELISTD,R5                                                       
         CLI   SENAME,C'A'         TEST ACCOUNT SYSTEM                          
         BNE   WHO19                                                            
         CLI   TACCS,C'T'          TALENT ACCESS CODES                          
         BNE   WHO19                                                            
         GOTO1 VHEXOUT,DMCB,TACCS+1,SRVAACC+1,2  EXPAND LEDGER/CPY              
         MVC   SRVAACC+5(1),TACCS+3                                             
*&&                                                                             
WHO19    MVC   SRVUPDT(3),=C'Yes'                                               
         TM    TTEST,X'80'         UPDATE                                       
         BZ    WHO20                                                            
         MVC   SRVUPDT,=C'No'                                                   
         TM    TTEST,X'10'         TEST IF TACCS IS A(TSTTAB)                   
         BZ    WHO20                                                            
         ICM   RF,15,TACCS                                                      
         USING TSTTABD,RF                                                       
         OC    TSTLOW,TSTLOW                                                    
         BZ    *+10                                                             
         MVC   SRVUPDT+2(4),=C',Log'                                            
         DROP  RF                                                               
*                                                                               
WHO20    OC    TPASSWD,TPASSWD     PASS NAME                                    
         BZ    WHO23                                                            
         TM    TFLAG,TFLAGSEC      DON'T DISPLAY SECRET CODES                   
         BO    WHO23                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'T'                                                         
         MVC   KEY+23(2),TPASSWD                                                
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,KEY                
         CLI   8(R1),0                                                          
         BNE   WHO23                                                            
*                                                                               
         LA    R6,KEY                                                           
         USING CTTREC,R6                                                        
         LA    R6,CTTDATA                                                       
         SR    RF,RF                                                            
*                                                                               
WHO22    CLI   0(R6),0             LOOK FOR PASSIVE ELEMENT                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),3                                                          
         BE    WHO22A                                                           
         ICM   RF,1,1(R6)                                                       
         BZ    *+8                                                              
         BXH   R6,RF,WHO22                                                      
         DC    H'0'                                                             
*                                                                               
WHO22A   MVC   SRVPNME(10),2(R6)                                                
*                                                                               
WHO23    SR    R1,R1               SESSION INFORMATION                          
         IC    R1,TSESSION                                                      
         LA    R1,C'A'(R1)                                                      
         STC   R1,SRVSESN                                                       
*                                                                               
         MVI   SRVASWP,C'N'        AUTOSWAP INFORMATION                         
         TM    TSTAT8,TST8ASWP                                                  
         BZ    *+8                                                              
         MVI   SRVASWP,C'Y'                                                     
*                                                                               
*TERMINAL DATA                                                                  
*                                                                               
WHO24    EDIT  (B2,TNUM),(4,SRVTNO),ALIGN=LEFT                                  
         MVC   SRVLUID,TLUID       SHOW LUID                                    
*                                                                               
         MVCDD SRVTTY,SR#PRR                                                    
         OC    TPRNT,TPRNT                                                      
         BZ    WHO25                                                            
         TM    TTYPE,TTYPERMC                                                   
         BZ    WHO26                                                            
         MVCDD SRVTTY,SR#SHTL                                                   
         B     WHO26                                                            
*                                                                               
WHO25    MVC   SRVTTY,=CL10'3270'                                               
         TM    TTYPE,TTYPE327                                                   
         BO    WHO26                                                            
         MVC   SRVTTY,=CL10'ICC'                                                
         TM    TTYPE,TTYPEICC                                                   
         BO    WHO26                                                            
         MVC   SRVTTY,=CL10'TWX'                                                
         TM    TTYPE,TTYPETWX                                                   
         BO    WHO26                                                            
         MVC   SRVTTY,=CL10'RJE'                                                
         TM    TTYPE,TTYPE378                                                   
         BO    WHO26                                                            
         MVC   SRVTTY,=CL10'COURIER'                                            
*                                                                               
WHO26    OC    TNUM,TNUM           UNLESS NOT IN UTL                            
         BZ    WHOXT                                                            
         MVC   SRVOFFC,TOFFCODE    OFFICE CODE                                  
*                                                                               
         MVC   DUB(4),DUTLA        A(UTL)                                       
         GOTO1 VHEXOUT,DMCB,DUB,SRVAUTL,4,0                                     
*                                                                               
         ICM   R0,15,TBUFF         A(TBUFF)                                     
         BZ    WHOXT                                                            
         ST    R0,DUB                                                           
         GOTO1 VHEXOUT,DMCB,DUB,SRVATBU,4                                       
*                                                                               
WHOXT    XMOD1 1                                                                
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
WHOWRKD  DSECT                                                                  
WHORELO  DS    A                                                                
ASYS     DS    A                                                                
KEY      DS    CL25                                                             
IO       DS    2000C                                                            
WHOWORKL EQU   *-WHOWRKD                                                        
*                                                                               
NDUMP    CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SECTION THAT SHOWS THE PARAMETER LIST                                         
***********************************************************************         
PARMSHOW DS    0H                                                               
         NMOD1 0,**PSHW**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            A(TWA)                                       
*                                                                               
         LA    R2,SRVTAGH          POINT TO WHERE SCREEN WILL GO                
         GOTO1 VCALLOV,DMCB,(X'FD',(R2)),0                                      
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         MVC   SRVMSG+7(28),=C'List of parameters displayed'                    
*                                                                               
         CLI   SRVP2H+5,0          NOTHING IN FIELD 2?                          
         BE    PSHW20              NO JUST DISPLAY THE LIST                     
*                                                                               
         OI    SAVEDFLG,X'40'      WE HAVE A PARAMETER LIST NOW                 
*                                                                               
         LA    R7,INA              FIND RESIDUAL L'REC FROM TWA START           
         A     R7,=A(RLEN)                                                      
         SR    R7,R5               R7 = REMAINING L'RECORD                      
         CH    R7,=Y(NDMPPARQ*L'NDMPPARM)   CAN COPY ALL PARAMETERS?            
         BNL   PSHW10              YES                                          
*                                                                               
         L     R3,SAVEAREA                                                      
         USING NDMPSAVD,R3                                                      
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   NDMPPARM(0),0(R5)   COPY WHAT WE HAVE FIRST                      
         LA    R7,1(R7)            R7 = NUMBER OF BYTES COPIED                  
*                                                                               
         LA    RF,INA              HOLD BYTES AT END OF LAST REC                
         A     RF,=A(RLEN-16)                                                   
         MVC   INBACK,0(RF)                                                     
         S     R5,=A(RLEN)                                                      
         XC    INA(16),INA                                                      
*                                                                               
         OC    NBLKS2,NBLKS2       IF NO MORE BLOCKS                            
         BZ    PSHW15              THEN NO MORE DATA LEFT, ASSUME 00'S          
*                                                                               
         LH    R0,NBLKS2           DECREMENT NUMBER OF BLOCKS LEFT              
         BCTR  R0,R0                                                            
         STH   R0,NBLKS2                                                        
*                                                                               
         BRAS  RE,READDSK          READ THIS BLOCK                              
         BNZ   DSKERR                                                           
*                                                                               
         AR    R5,R7               COPY WHAT IS LEFT                            
         LH    R1,=Y(NDMPPARQ*L'NDMPPARM)                                       
         SR    R1,R7                                                            
         LA    R7,NDMPPARM(R7)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PSHW15                                                           
         MVC   0(0,R7),0(R5)                                                    
         DROP  R3                                                               
*                                                                               
PSHW10   L     R1,SAVEAREA                                                      
         USING NDMPSAVD,R1                                                      
         MVC   NDMPPARM(NDMPPARQ*L'NDMPPARM),0(R5)                              
         DROP  R1                                                               
*                                                                               
PSHW15   GOTO1 =A(WRITTWAB),DMCB,(RC),(R9),RR=RELO   WRITE BEFORE READ          
*                                                                               
PSHW20   LA    R2,DMPL1H                                                        
         USING DMPPARMD,R2                                                      
         LA    R2,DMPPNEXT                                                      
         MVC   DMPPPARM,=C'P#'                                                  
         MVC   DMPPCONT,=C'CONTENTS'                                            
         MVC   DMPPHEX,=C'1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6'                      
         MVC   DMPPALPH,=C'1234567890123456'                                    
*                                                                               
         LA    R2,DMPPNEXT                                                      
         MVC   DMPPPARM,=C'--'                                                  
         MVC   DMPPCONT,=C'--------'                                            
         MVC   DMPPHEX,=C'--------------------------------'                     
         MVC   DMPPALPH,=C'----------------'                                    
*                                                                               
         LA    R2,DMPPNEXT         R2 = 1ST LINE DISPLAY LINE                   
         L     R3,SAVEAREA                                                      
         USING NDMPSAVD,R3                                                      
         LA    R3,NDMPPARM         R3 = 1ST PARAMETER                           
         DROP  R3                                                               
         LA    R4,NDMPPARQ         R4 = PARAMETER COUNTDOWN                     
*                                                                               
PSHW30   LH    R1,=Y(NDMPPARQ)     SHOW P#                                      
         SR    R1,R4                                                            
         LA    R1,1(R1)                                                         
         MVI   DMPPPARM,C'P'                                                    
         STC   R1,DMPPPARM+1                                                    
         OI    DMPPPARM+1,X'F0'                                                 
*                                  SHOW CONTENTS OF THAT PARAMETER              
         GOTO1 VHEXOUT,DMCB,(R3),DMPPCONT,4                                     
*                                                                               
         MVC   DMCB,0(R3)                                                       
         BAS   RE,PARMADDR         CHECK IF PARAMETER IS AN ADDRESS             
         BE    PSHW35                                                           
         MVI   DMPPHEX,C'('                                                     
         MVC   DMPPHEX+1(4),0(R3)    IT'S NOT                                   
         TR    DMPPHEX+1(4),TRTAB      SEE IF IT IS VALID EBCDIC                
         MVI   DMPPHEX+5,C')'                                                   
         B     PSHW40                                                           
*                                                                               
PSHW35   L     R5,DMCB             R5 = WHERE ADDRESS BEGINS                    
*                                                                               
         BAS   RE,DISPPADR         DISPLAY PARAMETER ADDRESS                    
*                                                                               
PSHW40   OI    6(R2),X'80'                                                      
         LA    R3,L'NDMPPARM(R3)   R3 = NEXT PARAMETER IN LIST                  
         LA    R2,DMPPNEXT         R2 = NEXT LINE                               
         LA    R2,DMPPNEXT                                                      
         BCT   R4,PSHW30           LOOP UNTIL WE'VE DISPLAY ALL PARMS           
         DROP  R2                                                               
*                                                                               
         NI    SRVP1H+6,X'FF'-X'40' UNSET CURSOR                                
         OI    SRVP2H+6,X'40'                                                   
         B     EXMOD                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS IF CONTENTS OF DMCB IS A VALID DUMP ADDRESS.              
***********************************************************************         
PARMADDR NTR1                                                                   
         L     R3,DMCB                                                          
         LA    R3,0(R3)                                                         
*                                                                               
         S     R3,DSTRT            START ADDRESS BELOW MINIMUM?                 
         BM    PADRNO              YES, NO A GOOD ADDRESS                       
*                                                                               
         SRL   R3,RSHIFT           R3 = # OF 8K BLOCKS FROM BEGINNING           
*         SRL   R3,11               R3 = # OF 2K BLOCKS FROM BEGINNING          
*         SR    R2,R2                                                           
*         D     R2,=A(BLKFCTR)      R3 = BLOCK, WHERE THE ADDRESS IS ON         
         ST    R3,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
*                                                                               
         SR    R2,R2                                                            
         D     R2,RECTRK           R2 = BLOCK NUM ON TRK MINUS 1                
         STC   R2,ADDR+2           R3 = TRACK NUM AFTER HEADER                  
         LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR                                                     
         AR    R3,R2               R3 = TRK NUM (ABSOLUTE DISK ADDRESS)         
         STH   R3,ADDR                                                          
*                                                                               
         LH    R3,DRECS            R3 = # OF 2K DUMP PAGES IN DUMP FILE         
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)      R3 = # OF 8K RECORDS IN DUMP FILE            
         L     R2,BLKNUM                                                        
         LA    R2,1(R2)                                                         
         SR    R3,R2                                                            
         BM    PADRNO              ADDRESS OUT OF RANGE                         
         STH   R3,NBLKS2           USED TO COUNT DOWN BLOCKS READ               
*                                                                               
         L     R4,DMCB             READ DUMP BLOCKS                             
         S     R4,DSTRT                                                         
         SRDL  R4,RSHIFT           DIVIDE BY 8K                                 
         SRL   R5,32-RSHIFT        R5 = REMAINDER IS OFFSET INTO INA            
         LA    R5,INA(R5)                                                       
         BRAS  RE,READDSK          INITIAL READ                                 
         BNZ   PADRNO                                                           
*                                                                               
         ST    R5,DMCB             WHERE ADDRESS BEGINS                         
*                                                                               
PADRYES  B     YES                                                              
*                                                                               
PADRNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS PARAMETER ADDRESS ON THE SCREEN FOR 32 BYTES            
*                                                                               
* ON ENTRY:    (R2)                ADDRESS LINE                                 
*              (R5)                A(IN INA WHERE ADDRESS BEGINS)               
***********************************************************************         
DISPPADR NTR1                                                                   
         LA    R3,2                PRINT ONLY 2 LINES                           
*                                                                               
         USING DMPPARMD,R2                                                      
DPADR10  LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
         CH    R7,=H'16'           MORE THAN 16 THEN CONTINUE PRINTING          
         BH    DPADR20                                                          
*                                                                               
         LA    RF,INA              HOLD BYTES AT END OF LAST REC                
         A     RF,=A(RLEN-16)                                                   
         MVC   INBACK,0(RF)                                                     
         S     R5,=A(RLEN)                                                      
         XC    INA(16),INA                                                      
*                                                                               
         OC    NBLKS2,NBLKS2                                                    
         BZ    DPADRX                                                           
*                                                                               
         LH    R0,NBLKS2           DECREMENT NUMBER OF BLOCKS LEFT              
         BCTR  R0,R0                                                            
         STH   R0,NBLKS2                                                        
*                                                                               
         BRAS  RE,READDSK          READ FOR THIS BLOCK                          
         BNZ   DSKERR                                                           
*                                                                               
DPADR20  GOTO1 VHEXOUT,DMCB,(R5),DMPPHEX,16                                     
         MVC   DMPPALPH,0(R5)                                                   
         TR    DMPPALPH,TRTAB                                                   
*                                                                               
         LA    R2,DMPPNEXT         NEXT LINE                                    
         LA    R5,16(R5)                                                        
*                                                                               
         BCT   R3,DPADR10          PRINT THE 2ND LINE                           
*                                                                               
DPADRX   B     XIT                 AND WE'RE DONE                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LIST THE FACPAK TABLE NAMES FOR DUMP DISPLAY                        *         
***********************************************************************         
FPAK     DS    0H                                                               
         NMOD1 0,**FPAK**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            A(TWA)                                       
         CLC   =C'ND,L',SRVID+1    'FE' SCREEN USED?                            
         BNE   FPAK00              NOT 'FE' SCREEN                              
         MVC   REGHOLD(L'SRVP1),SRVP4   COPY THE PARAMETERS                     
         MVC   REGHOLD+16(L'SRVP1),SRVP1                                        
         MVC   REGHOLD+32(L'SRVP1),SRVP2                                        
         MVC   REGHOLD+48(L'SRVP1),SRVP3                                        
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(3),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,3          FOR LENGTH OF 3                              
         MVC   SRVP4,REGHOLD       COPY BACK PARAMETERS                         
         MVC   SRVP1,REGHOLD+16                                                 
         MVC   SRVP2,REGHOLD+32                                                 
         MVC   SRVP3,REGHOLD+48                                                 
         OI    SRVP4H+6,X'80'                                                   
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP2H+6,X'80'                                                   
         OI    SRVP3H+6,X'80'                                                   
FPAK00   MVC   SRVMSG+7(39),=C'List of Facpak table names and keywords'         
         LA    R2,SRVTAGH          POINT TO WHERE SCREEN WILL GO                
         GOTO1 VCALLOV,DMCB,(X'FD',(R2)),0                                      
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         LA    R5,1                START AT COL 2                               
FPAK10   LA    R4,DMPL2(R5)                                                     
         LA    R2,(DMPLHH-DMPL2H)/(L'DMPL2H+L'DMPL2) NUMBER OF LINES            
FPAK20   CLI   0(R3),X'FF'         END OF TABLE ?                               
         BE    FPAKEXIT            YES                                          
         MVC   0(8,R4),0(R3)       MOVE KEYWORD                                 
         LA    R3,KEYTABL(,R3)     NEXT KEYWORD                                 
         LA    R4,L'DMPL1H+L'DMPL1(,R4) NEXT LINE                               
         BCT   R2,FPAK20           FILL COLUMN                                  
         LA    R5,10(,R5)          START OF NEXT COLUMN                         
         CH    R5,=H'80'           DONE LAST COLUMN ?                           
         BL    FPAK10              NO - LOOP                                    
FPAKEXIT B     EXMOD               EXIT $ND                                     
         SPACE 2                                                                
         LTORG                                                                  
       ++INCLUDE SRDMPKEY                                                       
         EJECT                                                                  
***********************************************************************         
* READS FROM THE DUMP FILE                                            *         
***********************************************************************         
READ     DS    0H                                                               
         NMOD1 0,**RDMP**          CLEAR BUFFER BEFORE READING DISK             
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         LA    RE,INA              ADDR OF BUFFER TO CLEAR                      
         LH    RF,=AL2(RLEN)       NUMBER OF BYTES TO CLEAR                     
         XCEFL                                                                  
*                                                                               
         MVC   P1,VRDID            INITIALISE DADDS PARAMS FOR READ             
*                                                                               
         SR    RE,RE               GET BLOCK NUMBER FOR TRACK                   
         IC    RE,ADDR+2                                                        
         C     RE,RECTRK           READ BLOCK ON THIS TRACK?                    
         BE    RD1                 YES                                          
         LA    RE,1(RE)            NO, BUMP TO NEXT BLOCK(RECORD)               
         STC   RE,ADDR+2           SAVE IT FOR NEXT READ                        
         B     RD2                 READ THE RECORD                              
*                                                                               
RD1      ICM   RE,3,ADDR           BUMP TO NEXT TRACK                           
         LA    RE,1(RE)                                                         
         STH   RE,ADDR                                                          
         MVI   ADDR+2,1            SET BLOCK NUMBER TO FIRST ON TRACK           
*                                                                               
RD2      GOTO1 VDADDS,P1           READ RECORD FROM ADDRESS                     
         OC    P3(2),P3            SET CC TO EQUAL IF READ IS OKAY              
         B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRITE FLAG                                                          *         
***********************************************************************         
WRFL     DS    0H                                                               
         NMOD1 0,**WRFL**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
*                                                                               
         LA    RE,INA              ADDR OF BUFFER TO CLEAR                      
         LH    RF,=AL2(RLEN)       NUMBER OF BYTES TO CLEAR                     
         XCEFL                                                                  
*                                                                               
         USING SSBD,R6                                                          
         TWAXC SRVCLRH,PROT=Y                                                   
*                                                                               
         XR    R0,R0                                                            
         IC    R0,DUMPNUM          COPY THE DUMP NUMBER                         
         BCTR  R0,0                OFFSET OF DUMP NUMBER 1                      
         SR    R1,R1                                                            
         IC    R1,SSBDMPCY         R1=NUM OF CYLS IN DUMP                       
         MR    R0,R0               FIND WHICH CYLINDER DUMP IS IN               
         M     R0,TRKCYL           FIND WHICH TRACK DUMP IS IN                  
         LA    R1,1(R1)                  T = TRACK                              
         SLA   R1,16                     B = BLOCK                              
         ST    R1,ADDR             SAVE TTTTBB00 FOR READ OF DUMP               
*                                                                               
         SR    RE,RE               GET BLOCK NUMBER FOR TRACK                   
         IC    RE,ADDR+2                                                        
         C     RE,RECTRK           READ BLOCK ON THIS TRACK?                    
         BE    WF1                 YES                                          
         LA    RE,1(RE)            NO, BUMP TO NEXT BLOCK(RECORD)               
         STC   RE,ADDR+2           SAVE IT FOR NEXT READ                        
         B     WF2                 READ THE RECORD                              
*                                                                               
WF1      ICM   RE,3,ADDR           BUMP TO NEXT TRACK                           
         LA    RE,1(RE)                                                         
         STH   RE,ADDR                                                          
         MVI   ADDR+2,1            SET BLOCK NUMBER TO FIRST ON TRACK           
*                                                                               
WF2      MVC   P1,VRDID            INITIALISE DADDS PARAMS FOR READ             
         LA    RF,INA              A(RECORD AREA) TO 2ND PARAMETER              
         ST    RF,P2                                                            
         XC    P3,P3               NOT NEEDED, SET BY DADDS                     
         L     RE,VDMPFILE                                                      
         ST    RE,P4                                                            
         LA    RF,ADDR             DISK ADDRESS                                 
         ST    RF,P5               A(DISK ADDRESS) TO 5TH PARAMETER             
         GOTO1 VDADDS,P1                                                        
*                                                                               
         OC    P3(2),P3            SET CC TO EQUAL IF READ IS OKAY              
         BZ    *+6                                                              
         DC    H'0'                READ ERROR                                   
*                                                                               
         LA    RF,INA                                                           
         USING DMPHDRD,RF                                                       
         MVI   DMPSTAT,DMPSPROC                                                 
         CLI   IND,C'C'            FIXED?                                       
         BNE   *+8                                                              
         MVI   DMPSTAT,DMPSFIX                                                  
         CLI   IND,C'D'            UNDO?                                        
         BNE   *+10                                                             
         XC    DMPSTAT,DMPSTAT                                                  
         DROP  RF                                                               
*                                                                               
         MVC   P1,VWTID            WRITE HEADER RECORD                          
         LA    RE,INA                                                           
         ST    RE,P2                                                            
         GOTO1 VDADDS,P1                                                        
         OC    P3(2),P3            TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   IND,C'B'            PROCESS?                                     
         BNE   WF3                                                              
         BRAS  RE,SVFLAG                                                        
*                                                                               
WF3      CLI   IND,C'C'            FIXED?                                       
         BNE   WRFX                                                             
         MVI   IND,C'E'            RELEASE DUMP                                 
         BRAS  RE,SVFLAG                                                        
         CLI   IND,C'C'                                                         
*                                                                               
WRFX     XC    SRVMSG+7(L'SRVMSG-7),SRVMSG+7                                    
         MVC   SRVMSG+7(6),=C'Dump #'                                           
         EDIT  DUMPNUM,(2,SRVMSG+13),ZERO=NOBLANK,ALIGN=LEFT                    
         MVC   SRVMSG+17(14),=CL14'status set'                                  
         MVI   SRVP1H+5,0                                                       
         XC    SRVP1,SRVP1                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* READ TWA 11                                                         *         
***********************************************************************         
READTWAB DS    0H                                                               
         NMOD1 0,**RTWA**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     R6,ATIA                                                          
         USING SRSD,R6                                                          
*                                                                               
         MVC   DINDIC,DSTRT        GET THE DUMP INDICATOR                       
         MVI   DSTRT,0             SET DUMP INDICATOR OFF, COPIED               
         LA    R2,SRPAGENO         SET UP UNIQUE KEY FOR TWA 11                 
         SLL   R2,32-8                                                          
         ICM   R2,3,TERMNUMB       WITH THIS TERMINAL                           
         L     RF,VDATAMGR         FIND ADDRESS OF DATAMGR                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 (RF),DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),(R6)               
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
         LA    R6,SR$DUMP          POINT TO THE SAVED DATA                      
         ST    R6,SAVEAREA         SAVE THE ADDRESS FOR LATER                   
         OI    STATFLAG,TWA11BIT   OKAY TO READ FROM THE FILE                   
         USING NDMPSAVD,R6                                                      
         CLC   =C'NDUMP',NDMPWORD  DID I PUT SOMETHING IN TWAB?                 
         BNE   RDTWABBD            NO, BAD TWA B                                
         CLC   NDMPNUMB,DUMPNUM    DUMP REQUEST SAME AS SAVED?                  
         BNE   RDTWABBD            YES                                          
         CLC   NDMPTIME,DTIM       DUMPED ON THE SAME TIME?                     
         BNE   RDTWABBD            YES                                          
         OC    SAVEDFLG,NDMPFLAG   KEEP REGISTERS SAVED IN TWAB                 
         CLI   SRXCLEAR,1          SRX C?                                       
         BNE   RTWAEXIT            NO                                           
         NI    SAVEDFLG,X'FF'-X'80'  YES, DON'T SET REGISTERS                   
         B     RTWAEXIT                                                         
*                                                                               
RDTWABBD NI    STATFLAG,X'FF'-TWA11BIT   NOT THE SAME TWA B                     
         MVI   NDMPLCNT,0          NOTHING IN LIST NOW                          
         MVI   NDMPFLAG,0                                                       
         MVI   SAVEDFLG,0          SAVED REGS AND PARAMS NOT GOOD NOW           
         L     R3,DTCBE            A(TCB)                                       
         S     R3,DSTRT                                                         
         SRL   R3,11               R3=# OF 2K BLOCK FROM BEGINNING              
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)      R3=# OF 2K RECORDS/READ                      
         SR    R2,R2                                                            
         D     R2,RECTRK           R2=REC NUM ON TRK MINUS 1                    
         STC   R2,ADDR+2           R3=TRK NUM AFTER HEADER                      
         LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR                                                     
         AR    R3,R2               R3=TRK NUM (ABSOLUTE DISK ADDRESS)           
         STH   R3,ADDR                                                          
         L     R4,DTCBE            READ DUMP BLOCKS                             
         S     R4,DSTRT                                                         
         SRDL  R4,RSHIFT                                                        
         SRL   R5,32-RSHIFT        OFFSET INTO A(INA)                           
         LA    R5,INA(R5)                                                       
         BRAS  RE,READDSK          INITIAL READ                                 
         BNZ   DSKERR                                                           
         LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
         CH    R7,=H'16'           MORE THAN 16 THEN CONTINUE                   
         BH    RTWAXIT1                                                         
         LA    RF,INA              HOLD BYTES AT END OF LAST REC                
         A     RF,=A(RLEN-16)                                                   
         MVC   INBACK,0(RF)                                                     
         S     R5,=A(RLEN)                                                      
         XC    INA(16),INA                                                      
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
         USING TCBD,R5             OVERLAY                                      
RTWAXIT1 MVC   NDMPATWA,TCBTWA     LOAD ADDRESS OF TWA                          
         MVC   NDMPSYSN,TCBSWSOV   SAVE THE SYSTEM NUMBER FOR DICTATE           
         DROP  R5                                                               
RTWAEXIT B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE TWA 11                                                        *         
***********************************************************************         
WRITTWAB DS    0H                                                               
         NMOD1 0,**WTWA**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     R6,SAVEAREA                                                      
         USING NDMPSAVD,R6                                                      
         MVC   NDMPWORD,=C'NDUMP'  INDICATE DUMP SAVED                          
         MVC   NDMPNUMB,DUMPNUM                                                 
         MVC   NDMPTIME,DTIM                                                    
         CLI   IND,C'M'            AUTO LIST FUNCTION?                          
         BE    TWABCONT            YES                                          
         CLI   IND,C'L'            LIST FUNCTION?                               
         BE    TWABCONT            YES                                          
         MVC   NDMPFLAG,SAVEDFLG                                                
         MVC   NDMPREGS(64),REGHOLD                                             
TWABCONT L     R6,ATIA                                                          
         USING SRSD,R6                                                          
         LA    R2,SRPAGENO         SET UP UNIQUE KEY FOR TWA 11                 
         SLL   R2,32-8                                                          
         ICM   R2,3,TERMNUMB       WITH THIS TERMINAL                           
         L     RF,VDATAMGR         FIND ADDRESS OF DATAMGR                      
         GOTO1 (RF),DMCB,(X'00',=C'DMWRT'),=C'TEMPSTR',(R2),(R6)                
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
         B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SET SSB DUMP SAVE FLAG                                              *         
***********************************************************************         
         SPACE 1                                                                
SVFLAG   NTR1  BASE=*,LABEL=*                                                   
         L     R6,VSSB                                                          
         USING SSBD,R6                                                          
         LAM   R0,RF,SVZERO                                                     
         SAC   0                                                                
*                                                                               
         LA    R1,SRVCLRH          CLEAR SCREEN                                 
         USING FHD,R1                                                           
         XR    RF,RF                                                            
SVFL02   ICM   RF,1,FHLN                                                        
         BZ    SVFL04                                                           
         LR    RE,RF                                                            
         AHI   RE,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RE,-(FHDAD)                                                      
         EX    RE,*+4                                                           
         XC    FHDA(0),FHDA                                                     
         BXH   R1,RF,SVFL02                                                     
*                                                                               
SVFL04   MVC   HALF(1),SSBSYSFL    SAVE TEST/REP SYSTEM STATUS                  
         NI    HALF,FACITST+FACIREP                                             
*                                                                               
         LAM   R2,R2,SSBTBLET                                                   
         XR    R2,R2                                                            
         SAC   512                                                              
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
         CLI   DUMPNUM,1           FIRST ENTRY IS A SPECIAL (DUMP 1)            
         BE    SVFL14                                                           
         AHI   R2,TDLENQ                                                        
*                                                                               
SVFL06   ICM   RF,15,TDSYSNA       ANOTHER ENTRY IN LIST?                       
         BNZ   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         L     RF,=A(FACIDTAB)     FIND OUT IF ENTRY IS SAME TYPE               
         A     RF,RELO                                                          
         USING FACITABD,RF                                                      
*                                                                               
SVFL08   CLI   FACISN4,X'FF'       EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TDSYSNA,FACISN4     DOES ENTRY LOOK GOOD?                        
         BE    SVFL10                                                           
         AHI   RF,L'FACITAB                                                     
         B     SVFL08                                                           
*                                                                               
SVFL10   MVC   HALF+1(1),FACIFL                                                 
         NI    HALF+1,FACITST+FACIREP                                           
         CLC   HALF(1),HALF+1      SAME TYPE (ADV/REP/TEST) ?                   
         BNE   SVFL12                                                           
         DROP  RF                                                               
*                                                                               
         XR    R1,R1               REFERENCED DUMP WITHIN THIS SLOT?            
         IC    R1,DUMPNUM                                                       
         CLM   R1,15,TDDUMPFS                                                   
         BL    SVFL12                                                           
         CLM   R1,15,TDDUMPMX                                                   
         BH    SVFL12                                                           
         B     SVFL14                                                           
*                                                                               
SVFL12   AHI   R2,TDLENQ           NEXT ENTRY                                   
         B     SVFL06                                                           
*                                                                               
SVFL14   CLI   IND,C'E'            RELEASE DUMP?                                
         BE    SVFL18                                                           
*                                                                               
         ICM   R0,15,TDDUMPMX                                                   
         ICM   RF,15,TDDUMPFS                                                   
         SR    R0,RF               NUMBER OF DUMPS IN THIS FACPAK               
         STH   R0,HALF                                                          
         AHI   R0,1                (ONES BASED)                                 
         XR    R1,R1                                                            
         LA    RF,1                SET UP BIT TEST MASK                         
         SLL   RF,31                                                            
*                                                                               
SVFL16   SRL   RF,1                COUNT DUMPS CURRENTLY SAVED                  
         ICM   RE,15,TDDUMPST                                                   
         NR    RE,RF                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,SVFL16                                                        
*                                                                               
         LH    RE,HALF             RE=NUMBER OF DUMPS IN THIS FACPAK            
         SRL   RE,1                                                             
         CR    R1,RE                                                            
         BH    SVFLS50             CANT SAVE >50% OF DUMPS FOR FACPAK           
*                                                                               
SVFL18   MVC   SRVMSG+7(6),=C'Dump #'                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,DUMPNUM          USE AS MASK SHIFT COUNT                      
         ICM   R0,15,TDDUMPFS                                                   
         SR    R1,R0               R1 = INDEX INTO THIS TABLE                   
*                                                                               
         LA    RF,1                SET UP BIT TEST MASK                         
         SLL   RF,31                                                            
         SRL   RF,1                FIRST BIT USED FOR DUMP SUPPRESSION          
         SRL   RF,0(R1)                                                         
*                                                                               
         ICM   R1,15,TDDUMPST                                                   
         CLI   IND,C'E'            RELEASING DUMP?                              
         BE    SVFL26                                                           
*                                                                               
         MVC   SRVMSG+16(5),=C'saved'                                           
         NR    R1,RF               TEST FLAG STATE                              
         BNZ   SVFL24              ALREADY SAVED                                
*                                                                               
         LR    R0,RF               SAVE MASK BIT                                
SVFL22   ICM   RE,15,TDDUMPST                                                   
         LR    RF,R0                                                            
         OR    RF,RE                                                            
         CS    RE,RF,TDDUMPST                                                   
         BNE   SVFL22                                                           
         B     SVFL32                                                           
*                                                                               
SVFL24   MVC   SRVMSG+16(13),=C'already saved'                                  
         B     SVFL32                                                           
*                                                                               
SVFL26   MVC   SRVMSG+16(8),=C'released'                                        
         NR    R1,RF                                                            
         BZ    SVFL30              DUMP NOT ON                                  
*                                                                               
         LR    R0,RF                                                            
         X     R0,=XL4'FFFFFFFF'                                                
SVFL28   ICM   RE,15,TDDUMPST                                                   
         LR    RF,R0                                                            
         NR    RF,RE                                                            
         CS    RE,RF,TDDUMPST                                                   
         BNE   SVFL28                                                           
         B     SVFL32                                                           
*                                                                               
SVFL30   MVC   SRVMSG+16(16),=C'already released'                               
         B     SVFL32                                                           
*                                                                               
SVFL32   SAC   0                                                                
         EDIT  DUMPNUM,(2,SRVMSG+13),ZERO=NOBLANK,ALIGN=LEFT                    
         B     SVFLX                                                            
*                                                                               
SVFLS50  SAC   0                                                                
         LAM   R0,RF,SVZERO                                                     
         MVC   SRVMSG+L'ERRHDR(27),=C'Cant save >50% of the dumps'              
         MVC   SRVMSG(L'ERRHDR),ERRHDR                                          
         MVI   SRVMSG+8,C'('                                                    
         MVC   SRVMSG+9(L'SYSNAME),SYSNAME                                      
         LA    RF,SRVMSG+L'SYSNAME+9                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         MVC   SRVMSG+2(1),SYSCH                                                
*                                                                               
SVFLX    MVI   SRVP1H+5,0                                                       
         XC    SRVP1,SRVP1                                                      
         XMOD1 ,                                                                
*                                                                               
         LTORG                                                                  
SVZERO   DC    16F'0'                                                           
         DROP  R6                                                               
***********************************************************************         
* LIST ALL DUMPS IN DUMP FILE                                         *         
***********************************************************************         
         SPACE 1                                                                
NALL     DS    0H                                                               
         NMOD1 NALLWRKL,**NALL**,CLEAR=YES                                      
*                                                                               
         LR    R7,RC                                                            
         USING NALLWRKD,R7                                                      
         MVC   NALLP4,SRVP4        SAVE FIRST SCREEN FIELD                      
         MVC   NALLP1,SRVP1        SAVE 2ND SCREEN FIELD                        
         MVC   NALLP1H,SRVP1H      SAVE 2ND SCREEN FIELD & HEADER               
         MVC   NALLHDR,SRVMSG      SAVE 2ND SCREEN FIELD & HEADER               
*                                                                               
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            A(TWA)                                       
*                                                                               
         L     R1,ATIOB            LOAD A(TIOB)                                 
         MVC   NALLPFK,TIOBAID-TIOBD(R1)                                        
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'FC',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         USING T15DFCD,RA                                                       
*                                                                               
         MVC   ALLID(L'$ND),$ND    INDICATE REGULAR OPTION FOR FACPAK           
         LA    RF,ALLIDH                                                        
         OI    FHOI-FHD(RF),FHOITR TRANSMIT FIELD                               
         MVI   FHOL-FHD(RF),L'$ND  SET LENGTH                                   
         MVC   ALLP4,NALLP4        RESTORE FIRST FIELD INPUT                    
         LA    RF,ALLP4H                                                        
         OI    FHOI-FHD(RF),FHOITR                                              
*                                                                               
         MVC   ALLP1,NALLP1        RESTORE 2ND FIELD INPUT                      
         LA    RF,ALLP1H                                                        
         OI    FHOI-FHD(RF),FHOITR+FHOICU                                       
*                                                                               
         BAS   RE,NALLFLT          VALIDATE ANY FILTER SET                      
         BNE   NALLX                                                            
*                                                                               
         MVC   ALLMSG,NALLHDR                                                   
         CLI   IND,C'A'                                                         
         BNE   *+8                                                              
         BAS   RE,NALLMSG          SET HEADER MESSAGE + SERVICE FIELD           
*                                                                               
         L     R6,VSSB             GET DUMP INFO FROM SSB                       
         USING SSBD,R6                                                          
*                                                                               
         CLI   MASKFLAG,0          TEST FOR DUMP CONTROL REQUEST                
         BE    NALL0C                                                           
         CLI   MASKFLAG,C'Y'                                                    
         BNE   NALL0B                                                           
         OI    SSBDMPSV,X'80'      SET SSB STOP FLAG                            
         B     NALL0C                                                           
*                                                                               
NALL0B   CLI   MASKFLAG,C'N'                                                    
         BNE   NALL0C                                                           
         NI    SSBDMPSV,X'7F'      CLEAR SSB STOP FLAG                          
*                                                                               
NALL0C   TM    SSBDMPSV,X'80'      TEST SSB STOP FLAG                           
         BZ    NALL0D                                                           
         LA    RF,ALLMSG+L'ALLMSG-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVC   2(L'STOPMSG,RF),STOPMSG                                          
*                                                                               
NALL0D   MVI   MASKFLAG,0                                                       
         XC    DUMPINDX,DUMPINDX   COUNT OF DUMPS ON FILE                       
         LH    R4,=Y(SORTBLCK-WRKD) INTIALISE FOR XSORT                         
         AR    R4,RC                                                            
         USING ALIND,R4                                                         
*                                                                               
         OC    NALLALET,SSBTBLET   CHECK USING CORRECT FAABEND                  
         BNZ   *+6                                                              
         DC    H'0'                NO NEED NEW ABEND                            
*                                                                               
         STAR  CLEAR=ARZERO,ARS=ON      SAVE AR STATUS                          
         LAM   R2,R2,NALLALET      GET A(TABSDMP)                               
         XR    R2,R2                                                            
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
         LA    R2,TDLENQ(,R2)      FIRST SLOT NOT USED (LEGACY)                 
*                                                                               
NFL      OC    TDSYSNA,TDSYSNA     REACHED LAST FACPAK IN DATASPACE             
         BZ    NFLX                NO                                           
         LA    R1,FACIDTAB         SEE IF IT IS A FACPAK                        
         USING FACITABD,R1                                                      
*                                                                               
NFLL02   CLI   FACITAB,X'FF'       END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   FACISN4,TDSYSNA                                                  
         BE    *+12                                                             
         AHI   R1,L'FACITAB                                                     
         B     NFLL02                                                           
*                                                                               
         MVC   HALF(1),FACIFL                                                   
         NI    HALF,FACITST+FACIREP                                             
         MVC   HALF+1(1),SSBSYSFL                                               
         NI    HALF+1,FACITST+FACIREP                                           
         CLC   HALF(1),HALF+1                                                   
         BNE   NFL02                                                            
         DROP  R1                                                               
                                                                                
         LA    RF,1                RESET SAVED MASK                             
         SLL   RF,32-1                                                          
         STCM  RF,15,NALLMSK                                                    
         MVC   NALLTHIS,TDDUMPFS   SET CURRENT NUMBER TO FIRST                  
*                                                                               
NDL      XC    ALINLIN,ALINLIN     CLEAR CURRENT LINE                           
         L     RF,NALLMSK                                                       
         SRL   RF,1                SHIFT SAVE FLAG                              
         ST    RF,NALLMSK                                                       
*                                                                               
         ICM   RE,15,NALLTHIS      CALCULATE START DISK ADDRESS                 
         BCTR  RE,0                                                             
         XR    RF,RF                                                            
         IC    RF,SSBDMPCY         RF=NUM OF CYLS IN DUMP                       
         MR    RE,RE               FIND WHICH CYLINDER DUMP IS IN               
         M     RE,TRKCYL           FIND WHICH TRACK DUMP IS IN                  
         LA    RF,1(RF)                  T = TRACK                              
         SLA   RF,16                     B = BLOCK                              
         ST    RF,ADDRSTR          SAVE TTTTBB00 OF START OF DUMP               
         ST    RF,ADDR             SAVE TTTTBB00 FOR READ OF DUMP               
*                                                                               
         GOTO1 =A(READ),DMCB,(RC),(R9),RR=RELO                                  
         SAFE  CLEAR=Y                                                          
         BE    *+14                                                             
         MVC   ALINNUM(3),=C'N/A'  ERROR ON READ                                
         B     NDLX                                                             
         DROP  R6                                                               
*                                                                               
         LA    R0,DHDR                                                          
         LA    R1,DMPHDRL                                                       
         LA    RE,INA                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY HEADER TO OUR W/S                       
*                                                                               
         MVC   DINDIC,DSTRT        GET THE DUMP INDICATOR                       
         MVI   DSTRT,0             SET DUMP INDICATOR OFF, COPIED               
*                                                                               
         BAS   RE,NALDIS           DISPLAY LINE INTO SORT BLOCK                 
*                                                                               
NDLX     XR    RE,RE               INCREMENT COUNT OF DUMPS TO DISPLAY          
         IC    RE,DUMPINDX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DUMPINDX                                                      
*                                                                               
         LA    R4,LENLINE(R4)      NEXT LINE IN SORT BLOCK                      
*                                                                               
         L     RF,NALLTHIS         INCREMENT CURRENT DUMP NUMBER                
         LA    RF,1(RF)                                                         
         ST    RF,NALLTHIS                                                      
         CLM   RF,15,TDDUMPMX      MAX DUMPS FOR FACPAK?                        
         BNH   NDL                 NO -  NEXT DUMP                              
*                                                                               
NFL02    LA    R2,TDLENQ(,R2)      NEXT FACPAK IN DATASPACE                     
         B     NFL                                                              
*                                                                               
NFLX     REAR  ARS=OFF                                                          
*                                                                               
         LH    R4,=Y(SORTBLCK-WRKD) SORT & DISPLAY                              
         AR    R4,RC                                                            
         USING ALIND,R4                                                         
         LA    R0,LENLINE          SET UP XSORT PARAMETERS                      
         XR    RF,RF                                                            
         ICM   RF,1,DUMPINDX                                                    
         BZ    NALLX                                                            
         LA    R2,ALINTRM-ALINDAT                                               
         LA    R3,ALINDAT-ALINLIN                                               
         GOTO1 VXSORT,DMCB,(X'FF',(R4)),(RF),(R0),(R2),(R3)                     
         GOTO1 VDATCON,DMCB,(5,0),(3,FULL)                                      
*                                                                               
         XR    RF,RF                                                            
         IC    RF,DUMPINDX                                                      
         STH   RF,NALLCNT          COUNT OF DUMPS TO DISPLAY                    
         LA    RF,NALLP4+1         LOOK FOR A START NUMBER                      
         XR    R1,R1                                                            
*                                                                               
NSL02    CLI   0(RF),C'9'          START NUMBER?                                
         BH    NSL04               NO                                           
         CLI   0(RF),C'0'                                                       
         BL    NSL04                                                            
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         B     NSL02                                                            
*                                                                               
NSL04    LTR   R1,R1               NUMBER TO COMPARE?                           
         BZ    NSL20               NO                                           
         BCTR  R1,0                                                             
         LH    RF,NALLCNT                                                       
*                                                                               
NSL06    STH   RF,NALLCNT                                                       
         EX    R1,*+8              MATCH NUMBER IN HEADER                       
         BNE   NSL08                                                            
         CLC   NALLP4+1(0),ALINNUM                                              
         LTR   R1,R1                                                            
         BNZ   NSL20               LENGTH <>1                                   
         CLI   ALINNUM+1,C'0'                                                   
         BL    NSL20               NO SECOND DIGIT                              
*                                                                               
NSL08    LA    R4,LENLINE(R4)                                                   
         BCT   RF,NSL06                                                         
*                                                                               
         XR    RF,RF                RESET COUNT                                 
         IC    RF,DUMPINDX                                                      
         STH   RF,NALLCNT                                                       
         LH    R4,=Y(SORTBLCK-WRKD) START FROM BEGINNING                        
         AR    R4,RC                                                            
         USING ALIND,R4                                                         
*                                                                               
NSL20    LA    R3,ALLL1H                                                        
         USING FLDHDRD,R3                                                       
*                                                                               
         XC    NALLNOW,NALLNOW                                                  
         XR    R0,R0               SEE IF SCROLL REQUESTED                      
         ICM   R0,1,NALLPFK                                                     
         BZ    NALLDSP1                                                         
         CH    R0,=Y(12)                                                        
         BNH   *+8                                                              
         SH    R0,=Y(12)                                                        
         STCM  R0,1,NALLPFK                                                     
*                                                                               
         XR    RF,RF               ONLY 7 & 8 WILL SCROLL                       
         CLI   NALLPFK,7                                                        
         BNE   *+12                                                             
         BAS   RE,NALFSU                                                        
         B     NALLDSP1                                                         
*                                                                               
         CLI   NALLPFK,8                                                        
         BNE   NALLDSP1                                                         
         CLC   NALLCNT,=Y(NUMLINE-1) ONLY DOWN IF MORE THAN A SCREEN            
         BNH   NALLDSP1                                                         
         BAS   RE,NALFSD                                                        
*                                                                               
NALLDSP1 XC    NALLNOW,NALLNOW                                                  
         CLC   =C'N/A',ALINNUM                                                  
         BE    *+10                                                             
         MVC   ALLP4+1(L'ALINNUM),ALINNUM                                       
*                                                                               
NALLDSP  BAS   RE,NALLDOF          DO FILTERING                                 
         BNE   NALLDSP8            NO GOOD                                      
*                                                                               
         NI    FLDATB,(X'FF'-FATBHIGH)                                          
         CLC   ALINDAT,NALLFILL    CONVERT DATE TO DISPLAY CHARACTERS           
         BE    NALLDSP4                                                         
*                                                                               
         CLC   FULL(3),ALINDAT     DUMP FOR TODAY?                              
         BNE   NALLDSP2            YES                                          
         MVC   ALINDAT,LTODAY      SET 'TODAY' AS A WORD                        
         CLI   ALINSTA,C'?'        ASSIGNED YET?                                
         BNE   *+8                 YES                                          
         OI    FLDATB,FATBHIGH     HIGHLIGHT IT THEN                            
         B     NALLDSP4                                                         
*                                                                               
NALLDSP2 GOTO1 VDATCON,DMCB,(X'03',ALINDAT),(X'0C',ALINDAT)                     
*                                                                               
NALLDSP4 MVC   FLDDATA(LENLINE),0(R4)                                           
         OI    FLDOIND,FOUTTRN     LINE HEADER TRANSMIT BIT                     
         LA    R3,LENLINE+L'ALLL1H(R3)                                          
*                                                                               
NALLDSP6 LH    RF,NALLNOW          INCREMENT SCREEN LINES DISPLAYED             
         LA    RF,1(RF)                                                         
         CH    RF,=Y(NUMLINE)      SCOPE                                        
         BNL   NALLX                                                            
         STH   RF,NALLNOW                                                       
*                                                                               
NALLDSP8 LH    RF,NALLCNT          DECREMENT NUMBER OF LINES LEFT               
         SH    RF,=H'1'                                                         
         BNP   NALLX               NO MORE                                      
         STH   RF,NALLCNT                                                       
*                                                                               
         LA    R4,LENLINE(R4)                                                   
         B     NALLDSP                                                          
*                                                                               
NALLX    TM    STATFLAG,DSKERFLG   DISK ERROR?                                  
         BNZ   NALLDERR            NO                                           
         B     EXMOD               EXIT $ND                                     
*                                                                               
NALLDERR MVC   ALLMSG+L'ERRHDR(L'NDEMSG),NDEMSG                                 
         MVC   ALLMSG(L'ERRHDR),ERRHDR                                          
         MVI   ALLMSG+8,C'('                                                    
         MVC   ALLMSG+9(L'SYSNAME),SYSNAME                                      
         LA    RF,ALLMSG+L'SYSNAME+9                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         MVC   ALLMSG+2(1),SYSCH                                                
         B     EXMOD               EXIT $ND                                     
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
RDIND    DS    0H                  RETURN INDIRECT DUMP FILE ADDRESS            
         ST    RE,RETURN           IN RECORD BUFFER                             
         LA    R3,0(R3)                                                         
         AR    R3,R5                                                            
         ST    R3,START                                                         
         GOTO1 =A(RDADR),DMCB,(RC),(R9),RR=RELO                                 
         BNZ   RDINDX                                                           
         L     R3,START           GET ADDRESS FROM RECORD BUFFER                
         LA    R3,0(R3)                                                         
         L     R3,0(R3)                                                         
         LA    R3,0(R3)                                                         
         ST    R3,START                                                         
         GOTO1 =A(RDADR),DMCB,(RC),(R9),RR=RELO                                 
         BNZ   RDINDX                                                           
         L     R3,START           R3 IS DESIRED ADDRESS                         
         LA    R3,0(R3)                                                         
RDINDX   L     RE,RETURN                                                        
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* SET DUMP DETAILS INTO SORT BLOCK FOR CURRENT LINE                   *         
***********************************************************************         
         SPACE 1                                                                
HDR      USING DMPHDRD,DHDR                                                     
         USING ALIND,R4                                                         
*                                                                               
NALDIS   NTR1  ,                                                                
*                              *** DUMP NUMBER                                  
         EDIT  HDR.DMPNUM,(2,ALINNUM),ZERO=NOBLANK,ALIGN=LEFT                   
*                                                                               
*                              *** NAME OF DUMPING FACPAK                       
         MVC   ALINADV,HDR.DMPDESC+4                                            
*                                                                               
*                              *** SAVE STATUS                                  
         ICM   RE,15,TDDUMPST                                                   
         L     RF,NALLMSK                                                       
         NR    RE,RF                                                            
         BZ    *+8                                                              
         MVI   ALINDAT-1,C'*'      DUMP SAVED                                   
*                                                                               
*                              *** DUPLICATE COUNT                              
         CLC   HDR.DMPDUPS,=F'99'  WITHIN RANGE?                                
         BNH   *+14                                                             
         MVC   ALINDUP,=CL2'!!'    TOO MANY                                     
         B     NALD02                                                           
*                                                                               
         ICM   R0,15,HDR.DMPDUPS                                                
         EDIT  (R0),(2,ALINDUP),ZERO=NOBLANK,ALIGN=LEFT                         
*                                                                               
*                              *** PROGRESS INDICATOR                           
NALD02   MVC   ALINSTA,DSTAT                                                    
         CLI   ALINSTA,C' '                                                     
         BH    *+8                                                              
         MVI   ALINSTA,C'?'        NO PROGRESS YET!                             
*                                                                               
*                              *** DATE                                         
         MVC   ALINDAT,NALLFILL                                                 
         OC    HDR.DMPHDTE,HDR.DMPHDTE                                          
         BZ    NALD04                                                           
         XC    ALINDAT,ALINDAT                                                  
         MVC   ALINDAT(L'DMPHDTE),HDR.DMPHDTE                                   
*                                                                               
*                              *** TIME                                         
NALD04   GOTO1 VDATTIM,DMCB,(X'81',HDR.DMPTIME),WORK                            
         SAFE  CLEAR=Y                                                          
         MVC   ALINTIM+0(2),WORK+8                                              
         MVC   ALINTIM+3(2),WORK+10                                             
         MVC   ALINTIM+6(2),WORK+12                                             
         MVI   ALINTIM+2,C'.'                                                   
         MVI   ALINTIM+5,C'.'                                                   
*                                                                               
*                              *** TERMINAL LUID                                
         MVC   ALINTRM,HDR.DMPHTRM                                              
         TR    ALINTRM,TRTAB                                                    
*                                                                               
*                              *** SYSTEM                                       
         MVC   ALINSYS,HDR.DMPHSYS                                              
*&&US                                                                           
         CLC   =C'SPOT',ALINSYS   CHANGE SPOT2 TO SPT2                          
         BNE   *+10                                                             
         MVC   ALINSYS+2(2),HDR.DMPHSYS+3                                       
         CLC   =C'PRNT',ALINSYS   DITTO FOR PRNT                                
         BNE   *+10                                                             
         MVC   ALINSYS+2(2),HDR.DMPHSYS+3                                       
*&&                                                                             
         TR    ALINSYS,TRTAB                                                    
*                                                                               
*                              *** PROGRAM                                      
         MVC   ALINPRG,HDR.DMPHPRG                                              
         TR    ALINPRG,TRTAB                                                    
*                                                                               
*                              *** FAILING ROUTINE                              
         MVC   ALINSUB,HDR.DMPHRTN                                              
         TR    ALINSUB,TRTAB                                                    
*                                                                               
*                              *** OFFSET (PSW-RB)                              
         MVC   ALINDSP,NALLFILL                                                 
         L     R1,HDR.DMPPSWD+4                                                 
         LA    R1,0(R1)            ADDRESS OF NEXT INSTRUCTION                  
         L     RF,DRB                                                           
         LA    RF,0(RF)            CONTENTS OF BASE REGISTER FROM DUMP          
         SR    R1,RF               OFFSET OF NEXT INTRUCTION                    
         ST    R1,DUB                                                           
         OC    DUB(2),DUB                                                       
         BNE   NALD08              DISPLAY ONLY 4 DECIMAL PLACES                
*                                                                               
         GOTO1 VHEXOUT,DMCB,DUB+2,ALINDSP,2,=C'TOG'  SEND TO SCREEN             
         SAFE  CLEAR=Y                                                          
         LA    R1,ALINDSP                                                       
         LA    RE,1                                                             
         LA    RF,L'ALINDSP-2(R1)                                               
*                                                                               
NALD06   CLI   0(R1),C'0'          BLANK LEADING ZEROES                         
         BNE   NALD08                                                           
         MVI   0(R1),C' '                                                       
         BXLE  R1,RE,NALD06                                                     
*                                                                               
*                              *** ABEND CODE                                   
NALD08   MVC   ALINABC,DINDIC                                                   
         CLI   DINDIC,C'I'         PROGRAM CHECK                                
         BNE   *+8                                                              
         MVI   ALINABC,C'L'        LOOP                                         
*                                                                               
*                              *** CODE AT ABEND ADDRESS-6                      
         MVC   ALININS,NALLFILL                                                 
         OC    HDR.DMPHPW6,HDR.DMPHPW6                                          
         BZ    NALDISX                                                          
         MVC   WORK(L'DMPHPW6),HDR.DMPHPW6                                      
         GOTO1 VHEXOUT,DMCB,WORK,ALININS,9,=C'TOG'  SEND TO SCREEN              
         SAFE  CLEAR=Y                                                          
*                                                                               
NALDISX  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET MESSAGE AND SERVICE REQUEST FIELD (PRESERVE RE)                 *         
***********************************************************************         
         SPACE 1                                                                
NALLMSG  MVI   ALLMSG,C' '          CLEAR DOWN MESSAGE FIELD                    
         MVC   ALLMSG+1(L'ALLMSG-1),ALLMSG                                      
         LA    RF,ALLMSG            SET (FAC) + MESSAGE                         
         MVI   0(RF),C'('                                                       
         MVC   1(L'SYSNAME,RF),SYSNAME                                          
         LA    RF,L'SYSNAME(RF)                                                 
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C')'                                                       
         MVC   3(L'SUMMDSP,RF),SUMMDSP                                          
         LA    R0,ALLMSG                                                        
         SR    RF,R0                                                            
         LR    R0,RF                                                            
         LA    RF,ALLMSGH                                                       
         OI    FHOI-FHD(RF),FHOITR TRANSMIT FIELD                               
         STC   R0,FHOL-FHD(RF)     SET LENGTH                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FILTER FIELD                                         *         
* NTRY:                                                               *         
* EXIT: CC EQ  - FILTER SET                                           *         
*       CC NEQ - INVALID FILTER                                       *         
***********************************************************************         
         SPACE 1                                                                
NALLFLT  NTR1  ,                                                                
         LA    RF,NALLP1H                                                       
         USING FHD,RF                                                           
         XR    R0,R0                                                            
         ICM   R0,1,FHIL                                                        
         BZ    NALLFLTX            NO INPUT TO FIELD                            
*                                                                               
         LA    RF,NALLP1                                                        
         CLI   0(RF),C'/'          SYS/PROG SEPARATOR?                          
         BE    NALLFL02            YES                                          
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         LA    RF,NALLP1H                                                       
         XR    RE,RE                                                            
         IC    RE,FHIL                                                          
         BCTR  RE,0                                                             
         LA    R1,FACIDTAB         SEE IF IT IS A FACPAK                        
         USING FACITABD,R1                                                      
*                                                                               
NALLFF02 CLI   FACITAB,X'FF'       END OF TABLE                                 
         BE    NALLFL06                                                         
         EX    RE,*+8                                                           
         BE    NALLFF04                                                         
         CLC   NALLP1(0),FACISN4                                                
         LA    R1,L'FACITAB(R1)                                                 
         B     NALLFF02                                                         
*                                                                               
NALLFF04 EX    RE,*+4                                                           
         MVC   NALLFACM(0),NALLP1                                               
         LA    RE,1(RE)                                                         
         STC   RE,NALLFACL                                                      
         MVI   NALLFVL,C'F'        SET FACPAK                                   
         CR    RB,RB                                                            
         B     NALLFLTX                                                         
         DROP  R1                                                               
*                                                                               
NALLFL02 LR    R1,RF                                                            
         LA    RE,NALLP1                                                        
         SR    R1,RE                                                            
         STC   R1,NALLSYSL         LENGTH OF SYSTEM PORTION                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   NALLSYSM(0),0(RE)                                                
*                                                                               
         LA    RF,1(RF)                                                         
         LR    RE,RF                                                            
         SH    R0,=H'1'                                                         
         BNP   NALLFERR                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,*-4                                                           
*                                                                               
NALLFL04 LR    R1,RF                                                            
         SR    R1,RE                                                            
         STC   R1,NALLPRGL         LENGTH OF PROGRAM PORTION                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   NALLPRGM(0),0(RE)                                                
         B     NALLFL08                                                         
*                                                                               
NALLFL06 LA    RF,NALLP1H          COPY SYSTEM FILTER                           
         XR    RE,RE                                                            
         IC    RE,FHIL                                                          
         STC   RE,NALLSYSL                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NALLSYSM(0),NALLP1                                               
*                                                                               
NALLFL08 L     R1,VSELIST          TRY FILTER SYSTEM                            
         LH    R2,0(R1)                                                         
         L     R3,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         XR    RE,RE                                                            
         IC    RE,NALLSYSL                                                      
         BCTR  RE,0                                                             
*                                                                               
*&&US                                                                           
         CLC   =C'SPT',NALLSYSM    CHANGE SPT1 TO SPOT1                         
         BE    NALLFL10                                                         
         CLC   =C'PRT',NALLSYSM    DITTO FOR PRT                                
         BNE   NALLFS                                                           
*                                                                               
NALLFL10 MVC   DUB,NALLSYSM                                                     
         MVC   NALLSYSM(2),DUB                                                  
         MVI   NALLSYSM+2,C'O'     SP(O)TX                                      
         CLC   =C'PRT',DUB                                                      
         BNE   *+8                                                              
         MVI   NALLSYSM+2,C'N'     PR(N)TX                                      
         MVC   NALLSYSM+3(2),DUB+2                                              
         XR    RE,RE                                                            
         ICM   RE,1,NALLSYSL                                                    
         LA    RE,1(RE)                                                         
         STC   RE,NALLSYSL         INCREMENT LENGTH                             
         BCTR  RE,0                                                             
*&&                                                                             
NALLFS   EX    RE,*+8              TRY TO MATCH SYSTEM NAME                     
         BE    NALLFS02            MATCH                                        
         CLC   SENAME(0),NALLSYSM                                               
         BXLE  R1,R2,NALLFS                                                     
         B     NALLFERR            NOT A SYSTEM - ERROR                         
*                                                                               
NALLFS02 DS    0H                                                               
*&&US                                                                           
         CLC   =C'SPOT',NALLSYSM   CHANGE SPOT1 TO SPT1                         
         BE    NALLFS04                                                         
         CLC   =C'PRNT',NALLSYSM   DITTO FOR PRNT                               
         BNE   NALLFS06                                                         
*                                                                               
NALLFS04 MVC   NALLSYSM+2(2),NALLSYSM+3                                         
         XR    R1,R1                                                            
         ICM   R1,1,NALLSYSL                                                    
         BZ    NALLFS06                                                         
         BCTR  R1,0                                                             
         STC   R1,NALLSYSL         REDUCE LENGTH                                
*&&                                                                             
NALLFS06 CLI   NALLFVL,C'P'        PROGRAM TO FOLLOW?                           
         BE    NALLFP                                                           
         MVI   NALLFVL,C'S'        SET SYSTEM FILTER                            
         CR    RB,RB               SET CC EQUAL                                 
         B     NALLFLTX                                                         
*                                                                               
NALLFP   L     R1,SEPGMS           TRY TO MATCH PROGRAM NAME                    
         LH    R2,0(R1)                                                         
         L     R3,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1                                                       
         XR    RE,RE                                                            
         IC    RE,NALLPRGL                                                      
         BCTR  RE,0                                                             
*                                                                               
NALLFP02 EX    RE,*+8              TRY TO MATCH PROGRAM NAME                    
         BE    NALLFLTX            MATCH                                        
         CLC   PGMNAME(0),NALLPRGM                                              
         BXLE  R1,R2,NALLFP02                                                   
         B     NALLFERR            NOT A PROGRAM - ERROR                        
*                                                                               
NALLFERR MVI   ALLMSG,C' '          CLEAR DOWN MESSAGE FIELD                    
         MVC   ALLMSG+1(L'ALLMSG-1),ALLMSG                                      
         LA    RF,ALLMSG            SET (FAC) + MESSAGE                         
         MVI   0(RF),C'('                                                       
         MVC   1(L'SYSNAME,RF),SYSNAME                                          
         LA    RF,L'SYSNAME(RF)                                                 
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C')'                                                       
         MVC   3(L'FLTERR,RF),FLTERR                                            
         LA    R0,ALLMSG                                                        
         SR    RF,R0                                                            
         LR    R0,RF                                                            
         LA    RF,ALLMSGH                                                       
         OI    FHOI-FHD(RF),FHOITR TRANSMIT FIELD                               
         STC   R0,FHOL-FHD(RF)     SET LENGTH                                   
         CLI   *,FF                AND CC NOT EQUAL                             
*                                                                               
NALLFLTX XIT1  ,                                                                
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER OUT AN ENTIRE SCREENS WORTH OF DATA - GOING UPWARDS          *         
* NTRY: R4      = FIRST SCREEN LINE TO BEGIN FILTERING                *         
*       NALLCNT = NUMBER OF LINES LEFT TO DISPLAY                     *         
***********************************************************************         
         SPACE 1                                                                
NALFSU   ST    RE,NALLRE                                                        
         XC    NALLNOW,NALLNOW     CURRENT NUMBER OF LINES FILTERED             
*                                                                               
NALFU02  LH    RF,=Y(SORTBLCK-WRKD)                                             
         AR    RF,RC               RF=A(FIRST ENTRY IN SORTBLCK)                
         CR    R4,RF                                                            
         BNH   NALFSUX             REACHED FIRST ENTRY                          
*                                                                               
         BAS   RE,NALLDOF          FILTER OUT NEXT LINE                         
         BNE   NALFU04             LINE NOT VALID                               
*                                                                               
         LH    RF,NALLNOW          INCREMENT FILTERED COUNT                     
         LA    RF,1(RF)                                                         
         CH    RF,=Y(NUMLINE-1)    SCOPE                                        
         BH    NALFSUX                                                          
         STH   RF,NALLNOW                                                       
*                                                                               
NALFU04  LH    RF,NALLCNT          INCREMENT NUMBER OF LINES LEFT               
         LA    RF,1(RF)                                                         
         STH   RF,NALLCNT                                                       
*                                                                               
         SH    R4,=Y(LENLINE)      PREVIOUS LINE                                
         B     NALFU02                                                          
*                                                                               
NALFSUX  L     RE,NALLRE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER OUT AN ENTIRE SCREENS WORTH OF DATA                          *         
* NTRY: R4      = FIRST SCREEN LINE TO BEGIN FILTERING                *         
*       NALLCNT = NUMBER OF LINES LEFT TO DISPLAY                     *         
***********************************************************************         
         SPACE 1                                                                
NALFSD   ST    RE,NALLRE           CURRENT NUMBER OF LINES FILTERED             
         XC    NALLNOW,NALLNOW     CURRENT NUMBER OF LINES FILTERED             
*                                                                               
NALFD02  BAS   RE,NALLDOF          FILTER OUT NEXT LINE                         
         BNE   NALFD04             LINE NOT VALID                               
*                                                                               
         LH    RF,NALLNOW          INCREMENT SCREEN LINES DISPLAYED             
         LA    RF,1(RF)                                                         
         CH    RF,=Y(NUMLINE-1)    SCOPE                                        
         BH    NALFSDX                                                          
         STH   RF,NALLNOW                                                       
*                                                                               
NALFD04  LH    RF,NALLCNT          DECREMENT NUMBER OF LINES LEFT               
         SH    RF,=H'1'                                                         
         BNP   NALLX               NO MORE                                      
         STH   RF,NALLCNT                                                       
         LA    R4,LENLINE(R4)                                                   
         B     NALFD02                                                          
*                                                                               
NALFSDX  L     RE,NALLRE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DO FILTERING FOR THIS LINE                                          *         
* NTRY: R4 = SCREEN LINE TO FILTER ON                                 *         
* EXIT: CC EQ  - KEEP                                                 *         
*       CC NEQ - DISCARD                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING ALIND,R4                                                         
NALLDOF  NTR1  ,                                                                
         L     R2,VSSB                                                          
         USING SSBD,R2                                                          
*&&US                                                                           
         CLC   =C'TST',SSBSYSN4    IS THIS TST SYSTEM?                          
         BNE   NADF01              NO                                           
         CLI   NALLFACL,0          SPECIFIC FACPAK REQUESTED?                   
         BNE   NADF01              NO                                           
         CLC   =C'TST',ALINADV     FILTER ON TST SYSTEM SPECIAL                 
         BNE   NALLDOFX                                                         
*                                                                               
NADF01   CLC   =C'MEL',SSBSYSN4    IS THIS MEL SYSTEM?                          
         BNE   NADF01A             NO                                           
         CLI   NALLFACL,0          SPECIFIC FACPAK REQUESTED?                   
         BNE   NADF01A             NO                                           
         CLC   =C'MEL',ALINADV     FILTER ON MEL SYSTEM SPECIAL                 
         BNE   NALLDOFX                                                         
*                                                                               
NADF01A  DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
         CLC   =C'TST',SSBSYSN4    IS THIS TST SYSTEM?                          
         BNE   NADF01              NO                                           
         CLI   NALLFACL,0          SPECIFIC FACPAK REQUESTED?                   
         BNE   NADF01              NO                                           
         CLC   =C'TST',ALINADV     FILTER ON TST SYSTEM SPECIAL                 
         BNE   NALLDOFX                                                         
*                                                                               
NADF01   CLC   =C'TTS',SSBSYSN4    IS THIS TTS SYSTEM?                          
         BNE   NADF01A             NO                                           
         CLI   NALLFACL,0          SPECIFIC FACPAK REQUESTED?                   
         BNE   NADF01A             NO                                           
         CLC   =C'TTS',ALINADV     FILTER ON TTS SYSTEM SPECIAL                 
         BNE   NALLDOFX                                                         
*                                                                               
NADF01A  CLC   =C'NEW',SSBSYSN4    IS THIS NEW SYSTEM?                          
         BNE   NADF01B             NO                                           
         CLI   NALLFACL,0          SPECIFIC FACPAK REQUESTED?                   
         BNE   NADF01B             NO                                           
         CLC   =C'NEW',ALINADV     FILTER ON NEW SYSTEM SPECIAL                 
         BNE   NALLDOFX                                                         
*                                                                               
NADF01B  CLC   =C'Y2K',SSBSYSN4    IS THIS Y2K SYSTEM?                          
         BNE   NADF01C             NO                                           
         CLI   NALLFACL,0          SPECIFIC FACPAK REQUESTED?                   
         BNE   NADF01C             NO                                           
         CLC   =C'Y2K',ALINADV     FILTER ON Y2K SYSTEM SPECIAL                 
         BNE   NALLDOFX                                                         
*                                                                               
NADF01C  DS    0H                                                               
*&&                                                                             
         DROP  R2                                                               
         CLI   NALLFVL,0           FILTER?                                      
         BE    NALLDOFX            NO                                           
*                                                                               
         XR    RF,RF               FACPAK?                                      
         ICM   RF,1,NALLFACL                                                    
         BZ    NADF06                                                           
*                                                                               
         CLM   RF,1,=AL1(2)                                                     
         BNH   NADF04                                                           
*                                                                               
         CLC   NALLFACM(2),=C'AD'  ADV FACPAK?                                  
         BNE   NADF02              NO                                           
         CLC   ALINADV(2),=C'AD'                                                
         BNE   NALLDOFX            ADV1 BECOMES AD1                             
         BCTR  RF,0                                                             
         LA    RF,NALLFACM(RF)                                                  
         CLC   ALINADV+2(1),0(RF)                                               
         B     NALLDOFX                                                         
*                                                                               
NADF02   CLC   NALLFACM(2),=C'RE'  REP FACPAK?                                  
         BNE   NADF04              NO                                           
         CLC   ALINADV(2),=C'RE'                                                
         BNE   NALLDOFX            REPA BECOMES REA                             
         BCTR  RF,0                                                             
         LA    RF,NALLFACM(RF)                                                  
         CLC   ALINADV+2(1),0(RF)                                               
         B     NALLDOFX                                                         
*                                                                               
NADF04   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     NALLDOFX                                                         
         CLC   ALINADV(0),NALLFACM                                              
*                                                                               
NADF06   XR    RF,RF               SYSTEM?                                      
         ICM   RF,1,NALLSYSL                                                    
         BZ    NALLDOFX                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   NALLDOFX                                                         
         CLC   ALINSYS(0),NALLSYSM                                              
*                                                                               
         ICM   RF,1,NALLPRGL       SYSTEM/PROGRAM?                              
         BZ    NALLDOFX                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     NALLDOFX                                                         
         CLC   ALINPRG(0),NALLPRGM                                              
*                                                                               
NALLDOFX XIT1  ,                                                                
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
ARZERO   DC    16F'0'                                                           
SUMMDSP  DC    CL30'Dump file summary information'                              
FLTERR   DC    CL30'Invalid filter value'                                       
STOPMSG  DC    CL20'- dumps are stopped'                                        
$ND      DC    CL4'$ND'                                                         
LTODAY   DC    CL6'Today'                                                       
NDEMSG   DC    C'Disk error on dump file '                                      
NALLFILL DC    40C'.'                                                           
*                                                                               
       ++INCLUDE FACIDTAB                                                       
*                                                                               
         LTORG                                                                  
*                                                                               
NALLWRKD DSECT                                                                  
NALLALET DS    A                   ALET FOR TABS DATASPACE                      
NALLTHIS DS    F                   CURRENT COUNTER                              
NALLMSK  DS    F                   TEST MASK FOR SAVED DUMPS                    
NALLRE   DS    F                   SAVED RE                                     
*                                                                               
NALLCNT  DS    H                   NUMBER OF LINES TO CHECK                     
NALLNOW  DS    H                   NUMBER OF LINES DISPLAYED                    
NALLNXT  DS    CL2                 NEXT DUMP NUMBER TO DISPLAY                  
*                                                                               
NALLPFK  DS    X                   PFKEY PRESSED                                
*                                                                               
NALLHDR  DS    CL60                                                             
NALLP4   DS    CL16                FIRST FIELD                                  
NALLP1H  DS    CL8                 SECOND FIELD HEADER                          
NALLP1   DS    CL16                SECOND FIELD                                 
NALLFVL  DS    X                   'S' SYS 'P' SYS/PRG 'F' FAC                  
NALLSYSL DS    X                                                                
NALLSYSM DS    CL8                                                              
NALLPRGL DS    X                                                                
NALLPRGM DS    CL8                                                              
NALLFACL DS    X                                                                
NALLFACM DS    CL8                                                              
NALLWRKL EQU   *-NALLWRKD                                                       
*                                                                               
NDUMP    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* READS FROM THE DUMP FILE INTO CORE RECORD BLOCK WITH 256 BYTE RANGE *         
* ON ENTRY START=DUMP FILE LOGICAL ADDRESS                            *         
* ON EXIT  START=ADDRESS OF DATA IN CORE BUFFER                       *         
***********************************************************************         
RDADR    DS    0H                                                               
         NMOD1 0,**RDAD**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     R3,START            ADDRESS IN DUMP FILE                         
         L     R2,DSTRT            RELATIVE TO START OF DATA                    
         LA    R2,0(R2)                                                         
         SR    R3,R2                                                            
         BM    RDADRE                                                           
         SRL   R3,11               R3=# OF 2K BLOCK FROM BEGINNING              
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)      R3=# 8K RECORD BLOCKS                        
         ST    R3,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
*                                                                               
         SR    R2,R2               CALCULATE TTTTBB00 FOR DATA START            
         D     R2,RECTRK           R2=REC NUM ON TRK MINUS 1                    
         STC   R2,ADDR+2           R3=TRK NUM AFTER HEADER                      
         LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR        TTTTBB00 OF START OF DUMP                    
         AR    R3,R2               R3=TRK NUM (ABSOLUTE DISK ADDRESS)           
         STH   R3,ADDR                                                          
*                                                                               
         LH    R3,DRECS            CHECK IF DATA IN RANGE SPECIFIED             
         SR    R2,R2               IN DUMP HEADER                               
         D     R2,=A(BLKFCTR)                                                   
         L     R2,BLKNUM                                                        
         LA    R2,1(R2)                                                         
         SR    R3,R2                                                            
         BM    RDADRE                                                           
*                                                                               
         L     R4,START            CALCULATE ADDRESS IN CORE BUFFER             
         L     R2,DSTRT                                                         
         LA    R2,0(R2)                                                         
         SR    R4,R2                                                            
         SRDL  R4,RSHIFT                                                        
         SRL   R5,32-RSHIFT                                                     
         LA    R5,INA(R5)                                                       
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
         LA    R7,INA              POINT TO THE DATA READ                       
         A     R7,=A(RLEN)         POINT BEYOND THE DATA                        
         SR    R7,R5               FIND GAP BTW THAT POINT AND CURRENT          
         CH    R7,=H'256'           MORE THAN 256 THEN CONTINUE                 
         BH    RDADR1                                                           
         LA    RF,INA              HOLD BYTES AT END OF LAST REC                
         A     RF,=A(RLEN-256)     AND ALLOW FOR 256 BYTE RANGE                 
         MVC   INBACKER,0(RF)      OF DATA IN CORE BUFFER                       
         S     R5,=A(RLEN)                                                      
         XC    INA(256),INA                                                     
         BRAS  RE,READDSK                                                       
         BNZ   DSKERR                                                           
RDADR1   ST    R5,START            OFFSET SAVED AS START ADDRESS                
         SR    R0,R0                                                            
         B     RDADRX                                                           
RDADRE   LA    R0,1                                                             
RDADRX   LTR   R0,R0                                                            
         B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCAN SCREEN INPUT FIELDS FOR HELP REQUEST                           *         
***********************************************************************         
         SPACE 1                                                                
HELPSCAN DS    0H                                                               
         NMOD1 0,**HELP**                                                       
         L     RC,0(R1)            WORK AREA                                    
         L     R9,4(R1)            SYSTEM FACILITES                             
         L     RA,8(R1)            A(TWA)                                       
         LA    R4,64(RA)           R4=A(FIRST FIELD)                            
         L     R6,ATIOB                                                         
         USING TIOBD,R6                                                         
         SR    R2,R2               CLEAR FIELD COUNT                            
SCAN1    SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         BZ    SCANX                                                            
         TM    1(R4),X'20'         TEST PROT                                    
         BNO   SCAN3                                                            
         AR    R4,R0               NEXT FIELD                                   
         B     SCAN1                                                            
SCAN2    LTR   R0,R0                                                            
         BZ    SCAN2A                                                           
         TM    HLPFLG,X'80'                                                     
         BNO   *+6                                                              
         BCTR  R1,0                                                             
         LR    R5,R0                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
SCAN2A   SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         AR    R4,R0                                                            
         B     SCAN1                                                            
*                                                                               
SCAN3    EQU   *                   UNPROT FOUND                                 
         LA    R2,1(R2)            INC FIELD COUNT                              
         LA    R1,8(R4)                                                         
         SH    R0,=H'8'                                                         
         SR    R5,R5               POS COUNT ZERO                               
SCAN4    CLI   0(R1),C'?'                                                       
         BE    SCAN5               HELP REQUIRED                                
         CLI   TIOBAID,1           CHECK FOR PFKEY 1                            
         BNE   SCAN4A                                                           
         SR    RF,RF               CHECK IF CURSOR AT THIS FIELD                
         LH    RF,TIOBCURD                                                      
         AR    RF,RA                                                            
         CR    RF,R4                                                            
         BE    SCAN5                                                            
SCAN4A   LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SCAN4                                                         
         B     SCAN2               NEXT FIELD                                   
*                                                                               
SCAN5    XC    HELP,HELP           CLEAR HELP                                   
         ST    R4,QHDR             SAVE ADDR                                    
         STC   R2,HLPFLD           SET FIELD NUM                                
         STC   R5,HLPPOS           POSITION                                     
         STC   R5,5(R4)            AND NEW FIELD LENGTH                         
         TM    DDS,DDSTRM                                                       
         BNO   SCAN6                                                            
         CLI   1(R1),C'*'          DDS CAN ENTER ?*                             
         BNE   SCAN6                                                            
         OI    HLPFLG,X'80'        AND GET DDS HELP                             
         LA    R1,1(R1)                                                         
SCAN6    SR    R3,R3               CHECK FOR PAGE NO                            
         TM    1(R1),X'F0'                                                      
         BNO   SCAN2                                                            
         LA    R3,1(R3)                                                         
         TM    2(R1),X'F0'                                                      
         BNO   SCAN7                                                            
         LA    R3,1(R3)                                                         
         TM    3(R1),X'F0'                                                      
         BNO   SCAN7                                                            
         LA    R3,1(R3)                                                         
*                                                                               
SCAN7    BCTR  R3,0                CONVERT PAGE NO                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R1)                                                      
         CVB   R3,DUB                                                           
         STC   R3,HLPPAG                                                        
         B     SCAN2                                                            
*                                                                               
SCANX    B     XIT                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
ATRACE   DS    0H                  WHERE TO START LOADING $TRA                  
       ++INCLUDE SRDMPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SRDMPDSECT                                                     
         EJECT                                                                  
       ++INCLUDE FATABSD                                                        
         EJECT                                                                  
       ++INCLUDE FATABSDMP                                                      
         EJECT                                                                  
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDFH                                                           
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE SRDDEQUS                                                       
         EJECT                                                                  
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111SRDMP00S  05/01/02'                                      
         END                                                                    
