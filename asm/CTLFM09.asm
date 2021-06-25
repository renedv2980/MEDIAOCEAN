*          DATA SET CTLFM09    AT LEVEL 020 AS OF 05/01/02                      
*PHASE TA0209A                                                                  
         TITLE 'CTLFM09 CONTROL FILE MAINT.- INTERFACE TP CTL RECORDS'          
CTLFM09  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 10,**LFM9**,RR=R8                                                
         ST    R8,RELO                                                          
         USING WRKD,RC             RC= A(TEMP W/S)                              
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTZREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*        VALIDATE KEY, ATTEMPT TO READ, DECIDE ON COURSE OF ACTION              
KEYVAL   LA    R1,TAPIDH           LA OF USER CODE ENTERED                      
         GOTO1 AFVAL               VALIDATE USER                                
         BZ    EXIT                NO THERE                                     
         CLI   FLDH+5,3            AT LEAST 3 BYTES LONG                        
         BL    EFTS                LESS, GO TO TOO SHORT ROUTINE                
         L     R5,AREC             ADDR OF RECORD                               
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY       CLEAR KEY                                    
         MVI   CTIKTYP,C'I'        SET FOR INFO RECORD                          
         MVC   CTIKID,FLD          PUT IN KEY                                   
         MVC   SVUSER,FLD          SAVE FOR RECORD                              
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               GO TO ROOT TO READ                           
         BZ    EXIT                ERROR-BAD READ                               
         CLI   DMCB+8,0            CHECK DMCB FOR RC                            
         BNE   ERNF                NOT FOUND, N.G.                              
         LA    R1,CTIDATA          GET TO ELEMENTS                              
         SR    RE,RE                                                            
         SPACE 1                                                                
KEYV2    CLI   0(R1),0             IS IT END OF RECORD                          
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         CLI   0(R1),X'02'         IS IT AN 02EL                                
         BE    *+14                YES                                          
         IC    RE,1(R1)            NO, UP TO WHERE NEXT SHOULD BE               
         AR    R1,RE                                                            
         B     KEYV2               GO CHECK THIS ONE                            
         DROP  R5                                                               
         SPACE 1                                                                
KEYV3    XC    CTZKEY,CTZKEY       DEVELOPE KEY & READ A RECORD                 
         MVI   CTZTYP,C'Z'                                                      
         MVC   CTZUSER,2(R1)       PUT ID FROM 02 EL                            
         LA    R1,TAPSYSH          VALIDATE SYSTEM                              
         GOTO1 AFVAL                                                            
         BZ    EXIT                NOT ENTERED                                  
         LA    R1,SYSTBL           MUST BE AT LEAST ONE BYTE IF THERE           
*                                  SO NO NEED TO CHECK FOR LENGTH               
         ZIC   RE,FLDH+5           L'FIELD                                      
         BCTR  RE,0                SUBT1                                        
KEYV3A   CLI   0(R1),X'00'         END                                          
         BE    EIIF                ERROR NOT ON TBL, INV FLD.                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(R1)        COMP FLD TO TBL                              
         BE    *+12                                                             
         LA    R1,L'SYSTBL(R1)     BUMP                                         
         B     KEYV3A              LOOP AGAIN                                   
*                                                                               
         MVC   TAPSYS(7),0(R1)        MOVE FULL NAME FOR DISPLAY                
         MVC   CTZSYS,0(R1)        MOVE TO KEY                                  
         OI    TAPSYSH+6,X'80'     TURN ON TRANSMIT BIT                         
         SPACE 1                                                                
         LA    R1,TAPPGMH                                                       
         GOTO1 AFVAL               NOW PROGRAM                                  
         BZ    EXIT                NOT THERE, N.G.                              
         CLI   FLDH+5,2            MUST BE 2 POS.                               
         BNE   EFTS                OTHERWISE IT IS TOO SHORT                    
         MVC   CTZPGM,FLD          MOVE TO KEY                                  
         SPACE 1                                                                
         LA    R1,TAPLGRH          NOW LEDGER                                   
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING, N.G.                                
         MVC   CTZLGR,FLD          LEDGER TO KEY                                
         SPACE 1                                                                
         MVC   KEY,CTZKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE         IF ACTN = CHANGE AND                         
         BNE   KEYV4               KEY NOT = TO LAST,                           
         CLC   KEY,LKEY            FORCE A                                      
         BE    KEYV4               DISPLAY FUNCTION                             
         MVI   ACTN,DISPLAY                                                     
         SPACE 1                                                                
KEYV4    CLI   ACTN,DISPLAY        SET RFU INDICATOR FOR NON-DISPLAY            
         BE    *+8                 FUNCTIONS                                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD               READ THE RECORD                              
         TM    DMCB+8,X'10'        FOUND                                        
         BZ    *+16                YES                                          
         CLI   ACTN,ADD            NOT FUNDS VALID ONLY FOR ADD                 
         BE    DATAVAL             VALID                                        
         B     ERNF                ERROR, NOT FOUND                             
         CLI   ACTN,ADD            IF FOUND RCD AND ACTN = ADD,                 
         BE    ERAE                ERROR, ALREADY EXISTS                        
         TM    DMCB+8,X'02'        DELETED RECORD                               
         BZ    *+12                NO                                           
         CLI   ACTN,DISPLAY        DELETED RECORD CAN ONLY BE DISPLAYED         
         BNE   ERID                ANY OTHER FUNCTION IS AN ERROR               
         CLI ACTN,CHANGE           RECORD EXISTS, IS ACTN CHANGE                
         BE    DATAVAL             YES                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DISPREC  DS    0H                                                               
         TWAXC TAPTYPLH            CLEAR TWA                                    
*                                                                               
         LA    R5,CTZDATA          GET TO ELEMENTS                              
         SPACE 1                                                                
DISPR2   CLI   0(R5),0             END OF RECORD                                
         BE    DISPIN2                                                          
         CLI   0(R5),X'B2'         INTERFACE DATA ELEMENT                       
         BE    DISPB2                                                           
         SPACE 1                                                                
DISPR3   SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,1(R5)            NECESSARY JUST TO REACH END OR IN            
         AR    R5,RE               CASE MORE ELEMENTS ARE ADDED IN THE          
         B     DISPR2              FUTURE                                       
         SPACE 1                                                                
DISPB2   DS    0H                  DISPLAY THE B2 FIELDS                        
         USING CTIFTD,R5           B2 DSECT                                     
         LA    R1,LBLWORDS         LOOK UP LABEL DET & TM BEFORE DATA           
DISPB2L  CLC   0(1,R1),CTIFTLTP     OPTION                                      
         BE    DISPB3                                                           
         LA    R1,L'LBLWORDS(R1)                                                
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   DISPB2L             NO                                           
         DC    H'0'                YES, DIE, TABLE DOES NOT MATCH               
*                                  RECORD. RECORD WAS CREATED FROM              
*                                  THIS TABLE SO THERE IS A SERIOUS             
*                                  PROBLEM IF THIS HAPPENS                      
DISPB3   MVC   TAPTYPL,1(R1)       PICKUP LABEL DATA IN ENGLISH                 
         MVC   TAPLTM,12(R1)        YES, NO,OR N/A FOR TMS.                     
         MVC   TAPLD1,CTIFTLD1     --FIRST 40 LABEL CHARS                       
         MVC   TAPLD2,CTIFTLD2     --SECOND 40                                  
         MVC   TAPTTM,CTIFTNTM     NO. OF TRAILING TMS                          
         MVC   TAPATTN,CTIFTATT    ATTN PERSON                                  
         CLI   CTIFTRPB,X'00'      VARIABLE LENGTH RECORDS HAVE RPB=X00         
         BNE   *+14                                                             
         MVC   TAPRPB,=C'VLR'                                                   
         B     DISPB4                                                           
         ZIC   R1,CTIFTRPB         RECORDS PER BLOCK                            
         CVD   R1,DOUBLE                                                        
         UNPK  DOUBLE,DOUBLE                                                    
         OI    DOUBLE+7,X'F0'      REMOVE ZONE                                  
         MVC   TAPRPB,DOUBLE+5                                                  
DISPB4   CLI   CTIFTSBP,X'00'      SHORT BLOCK CHAR                             
         BNE   *+14                                                             
         MVC   TAPSBPC,=C'NO'      NO SB PADDING                                
         B     *+10                                                             
         MVC   TAPSBPC(1),CTIFTSBP YES SB PADDING,MOVE CHAR                     
         SPACE 1                                                                
DISPIN2  DS    0H                                                               
         LA    R1,TAPTYPLH                                                      
         MVI   NACTN,OKCHA+OKDEL   OK TO UPDATE AND DELETE RECORD               
         TM    CTZSTAT,X'80'        UNLESS IT HAS BEEN DELETED                  
         BZ    *+8                 NO                                           
         MVI   NACTN,OKRES         YES ONLY ACTION ALLOWED IS TO                
*                                  REINSTATE IT.                                
         ST    R1,FADR             STORE R1 INTO FLD ADDR                       
         MVI   FERN,X'FF'          SET FERN TO OK STATUS                        
         B     EXIT                AND EXIT TO DISPLAY                          
         EJECT                                                                  
*     ADD/CHANGE B2 ELEMENT..............................                       
*                                                                               
DATAVAL  GOTO1 ABLDREC             BUILD KEY AND ACTIVITY EL (02) AND           
         GOTO1 ABLDACT             PUT IT INTO THE RECORD. ALL RECORDS          
         GOTO1 APUTEL              ARE BUILT FROM THE GROUND UP IN THIS         
*                                  PROGRAM SINCE THIS IS THE ONLY CTL           
*                                  FILE PROGRAM THAT TOUCHES THIS TYPE.         
         SPACE 1                                                                
*                                  BUILD B2 EL FROM A COMBINATION               
*                                  OF FIELDS THAT WERE CHANGED AND              
*                                  UNTOUCHED. VALIDATE ALL FIELDS AS            
*                                  IF THEY WERE CHANGED.                        
DATAV2   LA    R5,TEMP             LA OF ELEM BUILDING AREA                     
         MVI   CTIFTEL,X'B2'       SET EL CODE                                  
         MVI   CTIFTLEN,X'7E'      SET LENGTH (126)                             
         CLI   ACTN,CHANGE         IS THIS A CHANGE                             
         BNE   NOCHG               NO                                           
         GOTO1 ADELEL              YES, DELETE OLD ELEMENT                      
         SPACE 3                                                                
*                                                                               
*........LABEL TYPE................                                             
*                                                                               
NOCHG    LA    R1,TAPTYPLH         CHECK LABEL TYPE                             
         GOTO1 AFVAL               VALIDATE LABEL TYPE                          
         BZ    EXIT                NOT THERE                                    
         CLI   FLDH+5,6            MUST BE AT LEAST 6                           
         BL    EFTS                OR IT'S TOO SHORT                            
         MVC   SVLBLT,FLD                                                       
         SPACE 3                                                                
*                                                                               
*........TAPEMARK PRECEDING DATA...                                             
*                                                                               
         LA    R1,TAPLTMH          TM BEFORE DATA                               
         GOTO1 AFVAL                                                            
         BZ    NOTM               DEFAULT = NO                                  
         CLI   FLDH+5,2            ANSWER MUST BE AT LEAST 2                    
         BL    EFTS                                                             
         B     MVFLD                                                            
NOTM     MVC   SVNOTM,=C'NO '      FORCE NO IF OMITTED                          
         B     NOMV                                                             
MVFLD    MVC   SVNOTM,FLD          SAVE ANSWER                                  
*                                                                               
NOMV     CLC   =C'STD',SVLBLT                                                   
         BNE   *+10                                                             
         MVC   SVNOTM,=C'N/A'      N/A FOR STANDARD                             
         MVC   TAPLTM,SVNOTM       TRANSMIT FIELD                               
         OI    TAPLTMH+6,X'80'                                                  
         SPACE 2                                                                
         LA    R1,LBLWORDS                                                      
LBLLOOP  CLC   1(8,R1),SVLBLT                                                   
         BE    COMP2                                                            
LAR1     LA    R1,L'LBLWORDS(R1)                                                
         CLI   0(R1),X'FF'              END OF TBL                              
         BE    BADLBL                   YES INV LBL TYPE OR TM OPTION           
         B     LBLLOOP                  IN COMBINATION                          
COMP2    CLC   12(3,R1),SVNOTM          COMP LABEL TMS                          
         BE    MVTYPE                                                           
         B     LAR1                                                             
MVTYPE   MVC   CTIFTLTP,0(R1)           MOVE IN CODE                            
         B     DATAV3                   GO DO NEXT FIELD                        
BADLBL   LA    RE,TAPTYPLH              RESET CURSOR TO LABEL TYPE FLD,         
         ST    RE,FADR                   DONT KNOW WHICH IS IN ERROR,           
         B     EIIF                     TYPE OR TPMK.                           
*        CLEAR LABEL DATA AREA TO BLANKS IN 'B2' EL.                            
DATAV3   MVI   CTIFTLD1,C' '                                                    
         MVC   CTIFTLD1+1(79),CTIFTLD1                                          
*                                                                               
         SPACE 3                                                                
*                                                                               
*........LABEL DATA 1-40...........                                             
*                                                                               
         LA    R1,TAPLD1H          LABEL DATA 1                                 
         GOTO1 AFVAL                                                            
         BNZ   *+16                THERE                                        
         CLI   CTIFTLTP,C'4'       NOT THERE OK ONLY FOR NO LBLS.               
         BL    EXIT                NOT 4 OR 5, NOT VALID                        
         B     LD2                 NO LABELS, CHECK FOR LABEL DATA 2            
*                                                                               
         CLI   CTIFTLTP,C'0'                                                    
         BE    STDOS                                                            
         CLI   CTIFTLTP,C'1'       STANDARD LABELS                              
         BNE   FILLNS              NO                                           
STDOS    CLC   FLD(4),=C'HDR1'     IF IT IS A HDR, DON'T REBUILD                
         BE    FILLNS                                                           
         CLI   FLDH+5,17           ONLY 17 CHARS MAX FOR STD.                   
         BH    EFTL                MORE, FIELD TOO LONG                         
         MVI   CTIFTLD1,C' '       BLANK OUT LD 1 & 2.                          
         MVC   CTIFTLD1+1(79),CTIFTLD1                                          
         MVC   CTIFTLD1(4),=C'HDR1' BUILD HEADER                                
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CTIFTLD1+4(0),FLD        BUILD STANDARD LABEL FIELDS             
         MVC   CTIFTLD1+21,=6C'S'            DUMMY SERIAL NUMBER                
         MVC   CTIFTLD1+27(12),=3C'0001'                                        
         MVC   CTIFTLD1+39(1),=C'0'                                             
         MVI   CTIFTLD2,C'0'                                                    
         MVC   CTIFTLD2+1(12),=2C' YYDDD'    DUMMY CREATION/RETENTION           
         MVC   CTIFTLD2+13(7),=7C'0'                                            
         MVC   CTIFTLD2+20(8),=C'IBMDOSVS'                                      
         CLI   CTIFTLTP,C'0'                                                    
         BNE   *+10                                                             
         MVC   CTIFTLD2+20(8),=C'IBMOS   '                                      
         MVC   TAPLD1(40),CTIFTLD1                                              
         MVC   TAPLD2(40),CTIFTLD2                                              
         OI    TAPLD1H+6,X'80'     TRANSMIT                                     
         OI    TAPLD2H+6,X'80'     TRANSMIT                                     
         B     LD2                 STD LABEL IS BUILT                           
         SPACE 1                                                                
FILLNS   CLI   CTIFTLTP,C'4'       NO LABELS, LABEL DATA IS NOT ALLOWED         
         BNL   EIIF                                                             
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CTIFTLD1(0),FLD     MOVE NSD LABEL DATA AS IS                    
         SPACE 3                                                                
*                                                                               
*........LABEL DATA 41-80..........                                             
*                                                                               
LD2      LA    R1,TAPLD2H          LABEL DATA 2                                 
         GOTO1 AFVAL                                                            
         BZ    OKMISS              IF NOT THERE IT'S O.K. IN ANY CASE.          
         CLI   CTIFTLTP,C'2'       ALWAYS FOR TYPE 1,STD                        
         BL    MVELD2              IN CASE OF NON-LABEL CHANGE                  
         CLI   CTIFTLTP,C'3'       OR FOR TYPE 4,5 NO-LBL                       
         BH    EIIF                                                             
MVELD2   ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CTIFTLD2(0),FLD     MOVE IN LABEL DATA 2.                        
         SPACE 3                                                                
*                                                                               
*........EOF TAPE MARKS............                                             
*                                                                               
OKMISS   LA    R1,TAPTTMH          EOF TAPE MARKS                               
         GOTO1 AFVAL                                                            
         BZ    DEFT01              MISSING, DEFAULT TO 1                        
         TM    FLDH+4,X'08'        NUMERIC                                      
         BZ    EFNN                NO                                           
         MVC   CTIFTNTM,FLD        TAKE IT                                      
         CLI   CTIFTLTP,C'1'       STD LABELS                                   
         BNE   CHKRPB                NO                                         
DEFT01   MVI   CTIFTNTM,C'1'       DEFAULT TO 1 IF STD OR NOT I/P.              
         MVI   TAPTTM,C'1'         DEFAULT                                      
         OI    TAPTTMH+6,X'80'     TRANSMIT                                     
         SPACE 3                                                                
*                                                                               
*........RECORDS PER BLOCK.........                                             
*                                                                               
CHKRPB   LA    R1,TAPRPBH          RECORDS PER BLOCK                            
         GOTO1 AFVAL                                                            
         BNZ   *+24                MISSING IS BLOCK 1 DEFAULT                   
         MVC   CTIFTRPB,=X'01'                                                  
         MVC   TAPRPB,=C'001'                                                   
         OI    TAPRPBH+6,X'80'     TRANSMIT DEFAULT                             
         B     DATAV4                                                           
         CLC   FLD(3),=C'VLR'      VARIABLE LENGTH RECORDS                      
         BNE   *+12                                                             
         MVI   CTIFTRPB,X'00'      THEY GET RPB=X00                             
         B     DATAV4                                                           
         TM    FLDH+4,X'08'        NUMERIC                                      
         BZ    EIIF                NO                                           
         MVC   FULL,FLDH                                                        
         L     RE,FULL                                                          
         CH    RE,=H'255'          IS IT MORE THAN 255                          
         BH    EFTB                YES, FIELD VALUE TOO BIG                     
         MVC   CTIFTRPB,FLDH+3     LAST BYTE OF BINARY VALUE OF FIELD           
         SPACE 3                                                                
*                                                                               
*........SHORT BLOCK PAD CHAR......                                             
*                                                                               
DATAV4   LA    R1,TAPSBPCH         SHORT PAD CHAR.                              
         MVI   CTIFTSBP,0                                                       
         GOTO1 AFVAL                                                            
         BZ    DFLTV4            NOT THERE                                      
         CLI   FLDH+5,1            IS IT 1 CHAR LONG                            
         BNE   *+14                NO                                           
         MVC   CTIFTSBP,FLD        YES, USE IT                                  
         B     DATAV5                                                           
         CLC   FLD(2),=C'NO'       IS IT 'NO'                                   
         BNE   EIIF                NOT VALID                                    
         B     DATAV5                                                           
DFLTV4   MVC   TAPSBPC,=C'NO'      DEFAULT                                      
         OI    TAPSBPCH+6,X'80'    TRANSMIT                                     
         SPACE 3                                                                
*                                                                               
*........ATTN PERSON'S NAME......                                               
*                                                                               
DATAV5   LA    R1,TAPATTNH                                                      
         MVC   CTIFTATT,=20C' '    BLANK OUT EXISING NAME                       
         GOTO1 AFVAL                                                            
         BZ    DATAVEND            NOT THERE, IT'S O.K.                         
         MVC   CTIFTATT,FLD        MOVE IT IN IF THERE                          
         SPACE 1                                                                
* THIS IS THE END OF THE FIELD EDITING                                          
         SPACE 1                                                                
DATAVEND MVC   CTIFTUSR,SVUSER     ORIGINAL USER ID                             
         GOTO1 APUTEL              PUT GOOD NEW EL                              
         MVC   KEY,CTZKEY          SET KEY                                      
         L     RF,AADD             SET ADDR FOR ADD                             
         CLI   ACTN,CHANGE         WAS IT A CHANGE INSTEAD                      
         BNE   *+8                 NO                                           
         L     RF,AWRITE           SET ADDR FOR CHANGE REWRITE                  
         BASR  RE,RF               GO TO IT                                     
         CLI   DMCB+8,0            WAS IT DONE OK                               
         BNE   EIIO                NO                                           
         MVI   NACTN,OKDEL+OKCHA   SET NEXT ACTION (CHG,DEL)                    
         LA    R1,BASACTNH         SET CURSOR BASE SCREEN ACTION                
         ST    R1,FADR             FIELD..                                      
         MVI   FERN,X'FF'          SET FERN TO GOOD                             
         MVI   FNDX,0        ***** FORGOT WHAT THIS DOES *****                  
         B     EXIT                GET OUT                                      
         EJECT                                                                  
LBLWORDS DS    0CL15                                                            
         DC    C'0',C'STD*OS     ',C'N/A'                                       
         DC    C'1',C'STD*DOS    ',C'N/A'                                       
         DC    C'2',C'NONSTANDARD',C'YES'                                       
         DC    C'3',C'NONSTANDARD',C'NO '                                       
         DC    C'4',C'NO LABELS  ',C'YES'                                       
         DC    C'5',C'NO LABELS  ',C'NO '                                       
         DC    X'FF'                                                            
* ABOVE TBL IS C'TYPE,C'DESCRIPTION',C'TMS OR NOT'                              
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         SPACE 2                                                                
SYSTBL   DS    0CL12                                                            
*&&UK                                                                           
         DC    CL7'MEDIA  ',X'04',X'40000000'                                   
         DC    CL7'ACCOUNT',X'06',X'80000000'                                   
         DC    CL7'CONTROL',X'0A',X'00000000'                                   
         DC    CL7'GAMES  ',X'0B',X'00000000'                                   
         DC    CL7'CPP    ',X'0C',X'00000000'                                   
*&&                                                                             
*&&US                                                                           
         DC    CL7'SPOT   ',X'02',X'40000000'                                   
         DC    CL7'PRINT  ',X'04',X'40000000'                                   
         DC    CL7'ACCOUNT',X'06',X'80000000'                                   
         DC    CL7'REP    ',X'08',X'40000000'                                   
         DC    CL7'CONTROL',X'0A',X'00000000'                                   
         DC    CL7'GAMES  ',X'0B',X'00000000'                                   
         DC    CL7'CPP    ',X'0C',X'00000000'                                   
*&&                                                                             
SYSTBLX  DC    X'00'                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         DS    30H                PATCH AREA                                    
         EJECT                                                                  
         LTORG                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 2                                                                
WRKD     DSECT                                                                  
SVUSER   DS    CL10                                                             
SVLBLT   DS    CL8                                                              
SVNOTM   DS    CL3                                                              
DOUBLE   DS    D                                                                
FULL     DS    F                                                                
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF6D                                                                      
       ++INCLUDE CTLFMF6D                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTLFM09   05/01/02'                                      
         END                                                                    
