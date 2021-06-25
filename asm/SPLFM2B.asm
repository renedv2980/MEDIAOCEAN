*          DATA SET SPLFM2B    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T2192BA,+0                                                               
         TITLE 'T2192B - DEMO OVERRIDE DEFINITION'                              
T2192B   CSECT                                                                  
         PRINT  NOGEN                                                           
         NMOD1 0,T2192B                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R0,REC2             CLEAR WORK AREA IN REC2                      
         LA    R1,2000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
* INITIALIZE DBLOCK                                                             
         LA    RE,MYDBLOCK                                                      
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'       CANADIAN                                     
         DROP  RE                                                               
*                                                                               
         MVC   DMCB+4(4),=X'D9000AE0'   DEMOCON                                 
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOCON,0(R1)                                                   
*                                                                               
         MVC   DMCB+4(4),=X'D9000AD9'   DEMOVAL                                 
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOVAL,0(R1)                                                   
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING DOVRECD,R8                                                       
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
FMT10    LA    R2,REC+24                                                        
         CLI   0(R2),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST FIND 01 ELEM                            
*                                                                               
         GOTO1 VDATCON,DMCB,(3,DOVBBK),(6,WORK)                                 
         FOUT  DEMBSBKH,WORK,6                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(3,DOVUTBK),(6,WORK)                                
         FOUT  DEMUTBKH,WORK,6                                                  
*                                                                               
         BAS   RE,CLRSCR           CLEAR THE REST OF THE SCREEN                 
*                                                                               
FMT12    MVI   ELCODE,99           LOOK FOR OVERRIDE ELEMENT                    
         LA    R2,REC+24                                                        
         BAS   RE,NEXTEL                                                        
         BNE   FMT20                                                            
* GOT IT                                                                        
         MVC   DEMSHOW,2(R2)                                                    
         FOUT  DEMSHOWH                                                         
         B     EXIT                                                             
*                                                                               
FMT20    DS    0H                  FORMAT DEMO LIST                             
         LA    R2,REC+24                                                        
         SR    R4,R4                                                            
         ZIC   R5,1(R2)                                                         
         SH    R5,=H'12'                                                        
         BP    *+6                                                              
         DC    H'0'                INVALID RECORD                               
         D     R4,=F'3'            LEAVES R5 SET FOR BCT                        
         LA    R4,12(R2)           POINT TO FIRST DEMO                          
*                                                                               
         LA    R2,DEMDEM1H         POINT TO FIRST OUTPUT DEMO                   
*                                                                               
FMT22    DS    0H                                                               
         GOTO1 VDEMOCON,DMCB,(1,(R4)),(2,F),(C'S',MYDBLOCK),0                   
         XC    8(10,R2),8(R2)                                                   
         MVC   8(8,R2),F                                                        
         FOUT  (R2)                                                             
*                                                                               
         CLC   =C'USER',F          CHK FOR USER DEMO                            
         BNE   FMT24                                                            
         XC    8(10,R2),8(R2)                                                   
         MVC   8(3,R2),=C'U /'     FORMAT IS LIKE U1/                           
         MVC   9(1,R2),F+4         MOVE NUMBER                                  
         MVC   11(5,R2),F          MOVE NAME                                    
*                                                                               
FMT24    SR    R0,R0                                                            
         IC    R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
*                                                                               
         LA    R4,3(R4)            NEXT DEMO                                    
         OC    0(3,R4),0(R4)       TEST ANY MORE                                
         BZ    FMT30                                                            
         BCT   R5,FMT22                                                         
         EJECT                                                                  
FMT30    DS    0H                                                               
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMTX                NO 02 ELEM                                   
*                                                                               
         USING DOVEL02,R2                                                       
         SR    R4,R4                                                            
         IC    R4,1(R2)                                                         
         SH    R4,=H'2'                                                         
         SRDL  R4,32                                                            
         D     R4,=F'5'            R5 NOW SET FOR BCT                           
         LTR   R5,R5                                                            
         BZ    FMTX                NO DEMOS                                     
         LA    R4,DOVIMP           POINT TO IMP DEMO/VALUE                      
         DROP  R2                                                               
*                                                                               
         LA    R2,DEMIMP1H         POINT TO FIRST OUTPUT FIELD                  
*                                                                               
FMT42    CLI   1(R4),0             TEST NO MORE DEMOS                           
         BE    FMTX                                                             
*                                                                               
         XC    F,F                                                              
         GOTO1 VDEMOCON,DMCB,(1,(R4)),(2,F),(C'S',MYDBLOCK),0                   
*                                                                               
         LA    RE,F+7                                                           
         CLI   0(RE),C' '          BACK UP TO LAST NON-BLANK                    
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVI   1(RE),C'='                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,3(R4)          GET IMP VALUE                                
         EDIT  (R0),(6,2(RE)),1,ALIGN=LEFT                                      
*                                                                               
         MVC   8(14,R2),F                                                       
         FOUT  (R2)                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R4,5(R4)            NEXT IMP VALUE                               
         BCT   R5,FMT42            DECREMENT COUNT                              
*                                                                               
FMTX     B     EXIT                                                             
         EJECT                                                                  
*================================================================*              
* CLEAR FIELDS WITH DATA                                         *              
*================================================================*              
         SPACE 1                                                                
CLRSCR   NTR1                                                                   
         LA    R2,DEMSHOWH                                                      
*                                                                               
CLRSCR2  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   EXIT                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    CLRSCR2                                                          
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,FMTOC                                                         
         BE    CLRSCR2                                                          
         EX    RE,FMTCLC                                                        
         BE    CLRSCR2                                                          
         EX    RE,FMTXC                                                         
         OI    6(R2),X'80'         SET XMT                                      
         B     CLRSCR2                                                          
*                                                                               
FMTOC    OC    8(0,R2),8(R2)                                                    
FMTXC    XC    8(0,R2),8(R2)                                                    
FMTCLC   CLC   8(0,R2),SPACES                                                   
         EJECT                                                                  
EDT      DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,TODAYB)                                    
         BAS   RE,GETOTHR          GET 01/02 ELEMS FROM 'OTHER' REC             
*                                                                               
         XC    REC(256),REC        CLEAR RECORD AREA FOR ADD                    
         CLI   SVACT,C'A'                                                       
         BE    EDT2                                                             
* READ EXISTING RECORD                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
* NEED TO SET FLAG IF 05 ELEMENTS PRESENT                                       
         MVI   EL05FLAG,C'N'                                                    
         LA    R2,REC+24                                                        
         MVI   ELCODE,5                                                         
         BAS   RE,NEXTEL                                                        
         BNE   *+8                                                              
         MVI   EL05FLAG,C'Y'                                                    
*                                                                               
EDT2     XC    ELEM,ELEM           BUILD 01 ELEMENT IN ELEM                     
         MVI   ELEM,X'01'                                                       
         MVI   ELEM+1,12           SET MINIMUM LENGTH                           
*                                                                               
         LA    R7,ELEM                                                          
         USING DOVEL01,R7                                                       
         MVC   DOVCDAT,TODAYB      SET CREATION DATE                            
*                                                                               
EDT4     LA    R2,DEMBSBKH         EDIT BASE BOOK                               
         GOTO1 ANY                                                              
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         MVI   ERRCD,DATERR                                                     
         OC    DMCB(4),DMCB                                                     
         BZ    LFMERR                                                           
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         MVC   DOVBBK,WORK+10      Y/M                                          
*                                                                               
EDT6     LA    R2,DEMUTBKH         EDIT USE TILL BOOK                           
         GOTO1 ANY                                                              
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    LFMERR                                                           
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         MVC   DOVUTBK,WORK+10                                                  
         EJECT                                                                  
         LA    R2,DEMSHOWH                                                      
         CLI   5(R2),0             TEST OVRD SHOW ENTERED                       
         BNE   EDTOV               NO                                           
*                                                                               
EDT10    LA    R2,DEMDEM1H         POINT TO FIRST DEMO FIELD                    
         GOTO1 ANY                                                              
*                                                                               
EDT12    DS    0H                                                               
         GOTO1 VDEMOVAL,DMCB,(1,(R2)),(1,WORK),(C'S',MYDBLOCK),WORK2            
         CLI   DMCB+4,0                                                         
         BE    DEMERR                                                           
*                                                                               
         BAS   RE,TST01EL          TEST FOR DUPLICATE                           
         BNE   LFMERR                                                           
         BAS   RE,TSTINP           TEST IF INPUT PREVIOUSLY                     
         BNE   LFMERR                                                           
         BAS   RE,TSTOTHR          TEST IF IN OTHER RECORD                      
         BNE   LFMERR                                                           
* ADD DEMO TO ELEMENT                                                           
         ZIC   RE,ELEM+1                                                        
         LA    R1,ELEM(RE)         POINT TO END                                 
         MVC   0(3,R1),WORK        MOVE DEMO TO ELEMENT                         
         LA    RE,3(RE)                                                         
         STC   RE,ELEM+1           ADJUST ELEMENT LENGTH                        
*                                                                               
EDT14    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               NEXT DEMO FIELD                              
         LA    R0,DEMDEMXH                                                      
         CR    R2,R0                                                            
         BH    EDT20                                                            
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    EDT14               NO                                           
         B     EDT12                                                            
*                                                                               
EDT20    CLI   SVACT,C'A'                                                       
         BNE   EDT22                                                            
* ACTION IS ADD                                                                 
         ZIC   RE,ELEM+1           MOVE ELEMENT TO RECORD ON ADD                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   REC+24(0),ELEM                                                   
*                                                                               
         LA    RE,1(RE)            RESTORE ELEMENT LENGTH                       
         LA    RE,24(RE)           ADD RECORD OVERHEAD                          
         STCM  RE,3,REC+13         SET LENGTH IN RECORD                         
         B     EDT40                                                            
* ACTION IS NOT ADD                                                             
EDT22    DS    0H                                                               
         CLI   EL05FLAG,C'Y'       TEST 05 ELEMENTS PRESENT                     
         BE    EDT24                                                            
         EJECT                                                                  
*=================================================================*             
* IF NO 05 ELEMS, ALLOW CHANGE OF DEMOS                                         
*=================================================================*             
         SPACE 1                                                                
         LA    R2,REC+24                        RESET R2                        
         MVC   ELEM+2(3),2(R2)                  MOVE CREATION DATE              
         GOTO1 VRECUP,DMCB,(0,REC),(R2),0       DELETE OLD 01                   
         GOTO1 (RF),(R1),(0,REC),ELEM,(R2)      ADD NEW 01 ELEM                 
         BAS   RE,SETEOR                                                        
         B     EDT40                                                            
         SPACE 1                                                                
*=================================================================*             
* 05 ELEMENTS PRESENT. MAKE SURE THAT DEMOS THAT HAVE HAD VALUES  *             
* INPUT HAVE NOT CHANGED                                          *             
*=================================================================*             
         SPACE 1                                                                
EDT24    DS    0H                                                               
         MVI   CH01FLAG,C'N'       SET FLAG THAT 01 ELEM NOT CHANGED            
         LA    R2,REC+24           POINT TO OLD 01 ELEMENT                      
         CLC   ELEM+1(1),1(R2)     NEW ELEM LEN TO OLD                          
         BL    CHGERR              DEMOLIST CAN'T BE SHORTER                    
         BE    *+8                                                              
         MVI   CH01FLAG,C'Y'       SET FLAG ELEMENT CHANGED                     
*                                                                               
         MVC   ELEM+2(3),2(R2)     MOVE CREATION DATE FROM OLD TO NEW           
         GOTO1 VRECUP,DMCB,REC,(R2),0   DELETE OLD 01 ELEM                      
         GOTO1 (RF),(R1),REC,ELEM,(R2)  ADD NEW 01 ELEM                         
         BAS   RE,SETEOR                                                        
*                                  SEE IF I MUST CHG 05 ELEMS                   
         CLI   CH01FLAG,C'Y'       SEE IF 01 ELEMENT EXTENDED                   
         BNE   EDT40                                                            
         EJECT                                                                  
*=============================================================*                 
* WHEN 01 ELEMENT IS EXTENDED, MUST MAKE ROOM IN 05 ELEMENTS  *                 
* FOR ADDITIONAL DEMOS                                        *                 
*=============================================================*                 
         SPACE 1                                                                
         DS    0H                                                               
         LA    R2,REC+24           RESET R2                                     
         SR    R0,R0                                                            
         IC    R0,1(R2)            LENGTH OF 01 ELEM                            
         SH    R0,=H'12'                                                        
         SRDL  R0,32                                                            
         D     R0,=F'3'            R1  HAS NUMBER OF DEMOS                      
         AR    R1,R1               X 2                                          
         LA    R1,5(R1)            ADD 5 BYTES OVERHEAD                         
         STC   R1,ELEM+1           SET LENGTH IN ELEM BUILD AREA                
         MVI   ELEM,5              SET ELEMENT CODE                             
*                                                                               
         MVI   ELCODE,X'05'                                                     
EDT30    BAS   RE,NEXTEL                                                        
         BNE   EDT40                                                            
         ZIC   RE,1(R2)            OLD LENGTH                                   
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),2(R2)     MOVE DATA FROM OLD ELEM                      
*                                                                               
         GOTO1 VRECUP,DMCB,REC,(R2),0     DELETE OLD 05 ELEMENT                 
         GOTO1 (RF),(R1),REC,ELEM,(R2)    ADD NEW                               
         B     EDT30                                                            
         EJECT                                                                  
*===============================================================*               
* EDIT DEMO IMPRESSIONS OVERRIDES  FORMAT IS WM1849=123.4       *               
*===============================================================*               
         SPACE 1                                                                
EDT40    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         MVI   0(R7),X'02'                                                      
         MVI   1(R7),2             SET INITIAL LENGTH                           
*                                                                               
         LA    R2,DEMIMP1H                                                      
*                                                                               
EDT42    CLI   5(R2),0                                                          
         BE    EDT44                                                            
*                                                                               
         GOTO1 VSCANNER,DMCB,(R2),(1,BLK)                                       
SD       USING SCAND,BLK                                                        
         XC    FH,FH               BUILD DUMMY FLDHDR FOR DEMOVAL               
         XC    F,F                                                              
         LA    R0,L'FH+10          FLD1 LENGTH IS 10 MAX                        
         STC   R0,FH                                                            
         MVC   FH+5(1),SD.FLD1LEN                                               
         MVC   F(10),SD.FLD1                                                    
*                                                                               
         L     RF,VDEMOVAL                                                      
         LA    R1,DMCB                                                          
         GOTO1 (RF),(R1),(1,FH),(1,WORK),(C'S',MYDBLOCK),WORK2                  
         CLI   DMCB+4,0                                                         
         BE    DEMERR                                                           
         CLI   WORK+1,C'I'         MUST BE IMPRESSIONS                          
         BE    *+12                                                             
         CLI   WORK+1,X'21'        OR USER DEMO                                 
         BNE   DEMERR                                                           
*                                                                               
         BAS   RE,TST02EL          TEST FOR DUPLICATE THIS REC                  
         BNE   LFMERR                                                           
         BAS   RE,TSTOTHR          TEST FOR DUP - OTHER REC                     
         BNE   LFMERR                                                           
* ADD DEMO TO ELEMENT                                                           
         ZIC   RE,ELEM+1                                                        
         LA    R4,ELEM(RE)         POINT TO END                                 
         MVC   0(3,R4),WORK        MOVE DEMO TO ELEMENT                         
         LA    RE,5(RE)                                                         
         STC   RE,ELEM+1           ADJUST ELEMENT LENGTH                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SD.FLD2LEN                                                    
         GOTO1 VCASHVAL,DMCB,SD.FLD2,(R0)                                       
         DROP  SD                                                               
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BE    IMPERR                                                           
         ICM   R1,15,DMCB+4                                                     
         BNP   IMPERR                                                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         STCM  R1,3,3(R4)                                                       
*                                                                               
EDT44    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,DEMIMPX                                                       
         CR    R2,R0                                                            
         BH    EDT50                                                            
         CLI   5(R2),0                                                          
         BE    EDT44                                                            
         B     EDT42                                                            
*                                                                               
EDT50    DS    0H                                                               
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDT52                                                            
         GOTO1 VRECUP,DMCB,REC,(R2),0    DELETE 02 ELEMENT                      
*                                                                               
EDT52    LA    R2,REC+24           POINT TO 01 ELEMENT                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0               ADD 02 AFTER IT                              
         CLI   ELEM+1,2            TEST DATA IN 02 ELEMENT                      
         BNH   EDT100              NO - SKIP ADD                                
         GOTO1 VRECUP,DMCB,REC,ELEM,(R2)                                        
         EJECT                                                                  
EDT100   CLI   SVACT,C'A'                                                       
         BNE   EDT102                                                           
         MVC   REC(13),SVKEY                                                    
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     FMT10                                                            
*                                                                               
EDT102   DS    0H                                                               
         GOTO1 PUTREC                                                           
         B     FMT10               GO REFORMAT                                  
         EJECT                                                                  
*===============================================================*               
* OVERRIDE SHOW ENTERED - MAKE SURE IT EXISTS                   *               
* AND THAT THERE IS NO OTHER DATA IN THIS RECORD                *               
*===============================================================*               
         SPACE 1                                                                
EDTOV    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D12'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),SVNTWK     NETWORK                                      
         MVC   KEY+8(4),8(R2)      SHOW                                         
         OC    KEY+8(4),SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDTOVE1                                                          
* MAKE SURE NO OTHER INPUT FIELDS                                               
         MVI   ERRCD,INVERR                                                     
         CLC   KEY+8(4),SVKEY+7    MAKE SURE NOT ITSELF                         
         BE    LFMERR                                                           
         LA    R2,DEMDEM1H                                                      
         CLI   5(R2),0                                                          
         BNE   EDTOVE2                                                          
         LA    R2,DEMIMP1H                                                      
         CLI   5(R2),0                                                          
         BNE   EDTOVE2                                                          
* OK - BUILD RECORD NOW                                                         
         CLI   SVACT,C'A'          TEST ADD                                     
         BE    EDTOV2                                                           
         CLI   REC+24,1            TEST 01 ELEMENT PRESENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,REC+24                                                        
         USING DOVEL01,R7                                                       
         MVC   ELEM+2(3),DOVCDAT   MOVE CREATION DATE                           
*                                                                               
EDTOV2   XC    REC(256),REC                                                     
         MVC   REC(13),SVKEY                                                    
         MVC   REC+13(2),=H'24'                                                 
         MVC   REC+20(2),AGYALPHA                                               
         LA    R6,REC+24                                                        
         GOTO1 VRECUP,DMCB,REC,ELEM,(R6)                                        
* NOW SET ACTIVITY DATE                                                         
         LA    R7,REC+24                                                        
         USING DOVEL01,R7                                                       
         MVC   DOVADAT,TODAYB                                                   
* NOW ADD 99 ELEMENT                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,99                                                          
         MVI   ELEM+1,6                                                         
         MVC   ELEM+2(4),DEMSHOW                                                
         OC    ELEM+2(4),SPACES                                                 
         LA    R6,REC+24                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,REC,ELEM,(R6)                                        
         B     EDT100                                                           
*                                                                               
EDTOVE1  MVC   NERRCD,=AL2(NOSHOW)                                              
         B     EDTOVEX                                                          
*                                                                               
EDTOVE2  MVC   NERRCD,=AL2(NOSHWDTA)                                            
*                                                                               
EDTOVEX  DS    0H                                                               
         MVI   ERRCD,NEWERR                                                     
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     LFMERR                                                           
*                                                                               
CHGERR   MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(FEWDEMS)                                             
         LA    R2,DEMDEM1H         POINT TO FIRST OUTPUT DEMO                   
         B     LFMERR                                                           
*                                                                               
DEMERR   MVI   ERRCD,DEMINV                                                     
         B     LFMERR                                                           
*                                                                               
IMPERR   MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
         EJECT                                                                  
*=================================================================*             
* TEST IF DEMO VALUE IN WORK(3) IS IN ELEM YET                    *             
*=================================================================*             
         SPACE 1                                                                
TST01EL  NTR1                                                                   
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(DUP01ERR)                                            
         SR    R0,R0                                                            
         IC    R0,ELEM+1           GET NUMBER OF DEMOS                          
         SH    R0,=H'12'                                                        
         SRDL  R0,32                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R1               SET R0 FOR BCT                               
         BZ    EQXIT                                                            
         LA    R1,ELEM+12                                                       
*                                                                               
TST01EL2 CLC   WORK+1(2),1(R1)                                                  
         BE    NEQXIT                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,TST01EL2                                                      
         B     EQXIT                                                            
         EJECT                                                                  
*=================================================================*             
* TEST IF IMPRESSION DEMO IS IN ELEMENT YET                       *             
* AND MAKE SURE IT IS NOT IN THE 01 ELEMENT AS WELL               *             
*=================================================================*             
         SPACE 1                                                                
TST02EL  NTR1                                                                   
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(DUP02ERR)                                            
         SR    R0,R0                                                            
         IC    R0,ELEM+1           GET NUMBER OF DEMOS                          
         SH    R0,=H'2'                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LTR   R0,R1               SET R0 FOR BCT                               
         BZ    TST02EL4                                                         
         LA    R1,ELEM+2                                                        
*                                                                               
TST02EL2 CLC   WORK(3),0(R1)                                                    
         BE    NEQXIT                                                           
         LA    R1,5(R1)                                                         
         BCT   R0,TST02EL2                                                      
*                                                                               
TST02EL4 LA    R2,REC+24                                                        
         SR    R0,R0                                                            
         IC    R0,1(R2)            GET NUMBER OF DEMOS                          
         SH    R0,=H'12'                                                        
         SRDL  R0,32                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R1               SET R0 FOR BCT                               
         BZ    EQXIT                                                            
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(DUP01ERR)                                            
         LA    R1,12(R2)           POINT TO START OF DEMO LIST                  
*                                                                               
TST02EL6 CLC   WORK+1(2),1(R1)                                                  
         BE    NEQXIT                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,TST02EL6                                                      
         B     EQXIT                                                            
         EJECT                                                                  
*=================================================================*             
* IF DEMO VALUES PREVIOUSLY INPUT, MAKE SURE DEMO HAS NOT CHANGED *             
*=================================================================*             
         SPACE 1                                                                
TSTINP   NTR1                                                                   
         CLI   SVACT,C'A'          TEST ADD                                     
         BE    EQXIT                                                            
*                                                                               
         CLI   EL05FLAG,C'Y'       TEST 05 ELEMENTS PRESENT                     
         BNE   EQXIT                                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ELEM+1           GET LENGTH OF NEW ELEMENT                    
         LA    R1,REC+24           POINT TO 01 ELEMENT                          
         AR    R1,R0               POINT TO POSITION IN OLD ELEMENT             
         TM    0(R1),X'80'         TEST PREVIOUSLY INPUT                        
         BZ    EQXIT               NO                                           
         OI    WORK,X'80'          SET FLAG IN NEW DEMO                         
         CLC   WORK+1(2),1(R1)     TEST SAME DEMO CODE                          
         BE    EQXIT                                                            
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOCHGDEM)                                            
         B     NEQXIT                                                           
         EJECT                                                                  
*=================================================================*             
* GET RECORD WITH OTHER SEQUENCE NUMBER AND SAVE 01/02 ELEMENTS   *             
* ELCODE WILL BE REPLACED WITH NUMBER OF DEMOS IN SAVED ELEMENTS  *             
*=================================================================*             
         SPACE 1                                                                
GETOTHR  NTR1                                                                   
         XC    SVEL01,SVEL01                                                    
         XC    SVEL02,SVEL02                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         XI    KEY+12,1            SWITCH 1 TO 0 OR 0 TO 1                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                NOT FOUND                                    
*                                                                               
         GOTO1 GETREC                                                           
         LA    R2,REC+24                                                        
         CLI   0(R2),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVEL01(0),0(R2)                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SVEL01+1                                                      
         SH    R0,=H'12'                                                        
         SRDL  R0,32                                                            
         D     R0,=F'3'                                                         
         STC   R1,SVEL01           SAVE NUMBER OF DEMOS IN ELCODE               
*                                                                               
         SR    R0,R0                                                            
GETOTH2  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLI   0(R2),2                                                          
         BNE   GETOTH2                                                          
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVEL02(0),0(R2)                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SVEL02+1                                                      
         SH    R0,=H'2'                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         STC   R1,SVEL02           SAVE NUMBER OF DEMOS IN ELCODE               
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* TEST IF DEMO VALUE IN WORK(3) IS IN SAVED 01/02 ELEMENTS        *             
* IN 01 ELEM X'80' IN BYTE 0 IS USED AS INPUT FLAG                              
*=================================================================*             
         SPACE 1                                                                
TSTOTHR  NTR1                                                                   
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(OTHDMERR)                                            
*                                                                               
         CLI   SVEL01,0            TEST SAVED 01 ELEMENT                        
         BE    TSTOTH10            NO - TRY FOR 02                              
         SR    R0,R0                                                            
         IC    R0,SVEL01           GET NUMBER OF DEMOS                          
         LA    R1,SVEL01+12                                                     
*                                                                               
TSTOTH2  CLC   WORK+1(2),1(R1)                                                  
         BE    NEQXIT                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,TSTOTH2                                                       
*                                                                               
TSTOTH10 CLI   SVEL02,0            TEST SAVED 02 ELEMENT                        
         BE    EQXIT               NO                                           
         LA    R1,SVEL02+2                                                      
         SR    R0,R0                                                            
         IC    R0,SVEL02           GET NUMBER OF DEMOS                          
*                                                                               
TSTOTH12 CLC   WORK(3),0(R1)                                                    
         BE    NEQXIT                                                           
         LA    R1,5(R1)                                                         
         BCT   R0,TSTOTH12                                                      
         B     EQXIT                                                            
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BCR   8,RE                                                             
         CLI   0(R2),0                                                          
         BNE   *-18                                                             
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 2                                                                
SETEOR   DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,DOVLEN                                                      
         LA    RF,REC(RF)                                                       
         XC    0(2,RF),0(RF)       ZERO END OF RECORD                           
         BR    RE                                                               
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
GENOLD   DSECT                                                                  
         ORG   REC2                                                             
VDEMOCON DS    V                                                                
VDEMOVAL DS    V                                                                
EL05FLAG DS    XL1                                                              
CH01FLAG DS    XL1                                                              
         DS    CL2                                                              
FH       DS    XL8                                                              
F        DS    XL32                                                             
TODAYB   DS    XL3                                                              
         DS    XL5                                                              
*                                                                               
         DS    0D                                                               
MYDBLOCK DS    XL256                                                            
SVEL01   DS    XL256                                                            
SVEL02   DS    XL256                                                            
BLK      DS    XL256                                                            
         EJECT                                                                  
T219FFD  DSECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMEB                                                                        
       ++INCLUDE SPLFMEBD                                                       
         EJECT                                                                  
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENNDOV                                                      
         PRINT OFF                                                              
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPLFM2B   05/01/02'                                      
         END                                                                    
