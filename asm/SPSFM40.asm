*          DATA SET SPSFM40    AT LEVEL 038 AS OF 03/26/10                      
*PHASE T21740A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21740  -- SPILL DEF. RECORD MAINTENANCE             *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SPL DEF RECS ON SPTFILE                    *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMA5, AND SPSFMA6                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED SPL DEF RECS                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21740 - SPILL DEFINITION RECORD MAINTENANCE'                   
T21740   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1740**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE RECORD?                               
         BE    DELREC              YES                                          
         CLI   MODE,RECREST        RESTORE RECORD?                              
         BE    RESTREC             YES                                          
         CLI   MODE,XRECPUT                                                     
         BE    REQREC                                                           
         CLI   MODE,XRECADD                                                     
         BE    REQREC                                                           
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         B     LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         MVI   SVRSV,C' '                                                       
         XC    SVSTAT,SVSTAT                                                    
         XC    BCLT,BCLT                                                        
         XC    SVSTABMK,SVSTABMK                                                
         XC    SVLMKT,SVLMKT                                                    
         XC    TEMPFLD,TEMPFLD                                                  
         XC    SAVESEL,SAVESEL                                                  
         XC    SAVECLT,SAVECLT                                                  
         XC    SVMED,SVMED                                                      
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK50                                                             
*                                                                               
         LA    R2,SPLMEDH                                                       
         GOTO1 VALIMED                  VALIDATE MEDIA - REQUIRED               
         MVC   SVMED,QMED          SAVE MEDIA                                   
*                                                                               
* CODE FOR CLIENT AND MARKET FILTER INPUT                                       
*                                                                               
         LA    R2,SPLCLTH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK5                                                              
*                                                                               
         MVC   SAVECLT,SPLCLT                                                   
         CLC   SPLCLT,=C'ALL'                                                   
         BE    VK5                                                              
         GOTO1 VALICLT                                                          
*                                                                               
VK5      LA    R2,SPLMKTH                                                       
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         GOTO1 VALIMKT                                                          
         MVC   SVLMKT,BMKT                                                      
*                                                                               
VK10     LA    R2,SPLRATH                                                       
*                                                                               
         CLI   5(R2),0             OKAY IF NO ENTRY                             
         BE    VK100                                                            
*                                                                               
         MVI   SVRSV,C'1'                                                       
*                                                                               
         CLI   SVAPROF+7,C'C'           CANADIAN AGENCY?                        
         BNE   VKRSVCN                  NO, CHECK FOR US RATING SERV            
*                                                                               
         CLC   =C'BBM',SPLRAT           CHECK CANADIAN RTSV                     
         BE    VKRSVX                                                           
*                                                                               
         CLI   QMED,C'R'           ONLY ONE SERVICE FOR RADIO                   
         BE    ERRINV                                                           
*                                                                               
VK11     MVI   SVRSV,C'0'                                                       
         CLC   =C'CSI',SPLRAT                                                   
         BNE   ERRINV                                                           
         B     VKRSVX                                                           
*                                                                               
VKRSVCN  DS    0H                                                               
*                                                                               
         CLC   =C'ARB',SPLRAT                                                   
         BE    VKRSVX                                                           
*                                                                               
         CLI   QMED,C'R'           ONLY ONE SERVICE FOR RADIO                   
         BE    ERRINV                                                           
*                                                                               
VK13     MVI   SVRSV,C'0'                                                       
         CLC   =C'NSI',SPLRAT                                                   
         BNE   ERRINV                                                           
*                                                                               
VKRSVX   DS    0H                                                               
         LA    R2,SPLSTAH                                                       
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         CLI   5(R2),3                                                          
         BL    ERRINV                                                           
         GOTO1 VALISTA                                                          
         MVC   SVSTAT,QSTA              SAVE INPUT STATION                      
         OC    SVSTAT,SPACES                                                    
         B     VK100                                                            
*                                                                               
VK50     DS    0H             VALIDATE FOR DISPLAY/ADD/CHANGE                   
*                                                                               
         LA    R2,SPLMEDH                                                       
         GOTO1 VALIMED                  VALIDATE MEDIA                          
*                                                                               
*        VALIDATE RATING SERVICE                                                
*                                                                               
         LA    R2,SPMRATH          POINT TO RATING SERVICE FIELD                
*                                                                               
         CLI   5(R2),0             ENTRY REQUIRED                               
         BE    ERRMIS                                                           
*                                                                               
         MVI   SVRSV,C'1'          DEFAULT TO BBM OR ARB                        
*                                                                               
         CLI   SVAPROF+7,C'C'           CANADIAN AGENCY?                        
         BNE   VK52                     NO, CHECK FOR US RATING SERV            
*                                                                               
         CLC   =C'BBM',SPMRAT           CHECK CANADIAN RTSV                     
         BNE   VK51                                                             
         MVI   BKVALSRC,C'B'                                                    
         B     VK55                                                             
*                                                                               
VK51     MVI   SVRSV,C'0'                                                       
*                                                                               
         CLI   QMED,C'R'           ONLY 1 SERVICE IF RADIO                      
         BE    ERRINV                                                           
*                                                                               
         CLC   =C'CSI',SPMRAT                                                   
         BNE   ERRINV                                                           
         MVI   BKVALSRC,C'C'                                                    
         B     VK55                                                             
*                                                                               
VK52     CLC   =C'ARB',SPMRAT                                                   
         BNE   VK53                                                             
         MVI   BKVALSRC,C'A'            TO GET RTSV MKT LATER                   
         B     VK55                                                             
*                                                                               
VK53     MVI   SVRSV,C'0'                                                       
         CLI   QMED,C'R'           ONLY 1 SERVICE IF RADIO                      
         BE    ERRINV                                                           
*                                                                               
         CLC   =C'NSI',SPMRAT                                                   
         BNE   ERRINV                                                           
         MVI   BKVALSRC,C'N'                                                    
*                                                                               
VK55     DS    0H                                                               
*                                                                               
         LA    R2,SPMSTAH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   5(R2),3                                                          
         BL    ERRINV                                                           
         GOTO1 VALISTA                                                          
         MVC   SVSTAT,QSTA              SAVE INPUT STATION                      
         OC    SVSTAT,SPACES                                                    
         MVC   SVSTABMK,BMKT            SAVE STATION BINARY MARKET              
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   VK70                                                             
         CLI   ACTEQU,ACTADD                                                    
         BNE   VK70                                                             
         BAS   RE,CKCDNCBL         NO SPILL FOR CANADIAN CABLE                  
         BNE   ERCDNCBL                                                         
*                                                                               
VK70     LA    R2,SPMCLTH                                                       
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         GOTO1 VALICLT                                                          
*                                                                               
VK100    DS    0H                                                               
         LA    R4,KEY                                                           
         USING SDEFRECD,R4                                                      
         XC    KEY,KEY                                                          
         MVC   SDEFKTYP,=X'0D13'                                                
         MVC   SDEFKAGY,AGENCY                                                  
         MVC   SDEFKRSV,SVRSV                                                   
         MVC   SDEFKSTA,SVSTAT                                                  
*                                                                               
         CLI   QMED,C'R'           IF MEDIA RADIO                               
         BE    *+8                                                              
         MVI   SDEFKSTA+4,0           KEEP MEDIA IN STATION                     
*                                                                               
         MVC   SDEFKCLT,BCLT                                                    
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         DROP  R4                                                               
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVC   MYKEY,KEY                                                        
         XC    LOCALIO,LOCALIO                                                  
VR1      CLI   SVAPROF+7,C'C'                                                   
         BNE   *+12                                                             
         BAS   RE,RDAGYSPL         GET AGY LEVEL SPILL REC (CANADA)             
         BNE   ERRNOAGY            (BEFORE CLEAR OUT 05 ELEMS)                  
*                                                                               
         XC    ADDMKTS(ADMKLNEQ),ADDMKTS                                        
         CLI   ACTEQU,ACTADD                                                    
         BE    VR10                                                             
*                                                                               
         MVI   ELCODE,X'05'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VR10     DS    0H                                                               
         LA    R2,SPMMK1H                                                       
         LA    R3,ADDMKTS               SAVE A(FIRST TABLE ENTRY)               
*                                                                               
VR11     CLI   5(R2),0             NO INPUT IN AGY MKT FIELD                    
         BE    VR30                     SKIP TO NEXT MKT FIELD                  
         CLC   8(3,R2),=C'DEL'                                                  
         BE    VR30                     SKIP TO NEXT MKT FIELD                  
         CLI   ACTNUM,ACTSEL                                                    
         BNE   VR12                                                             
         TM    STAVERR,BASERR      DDS BYPASS STATION VAL SO CAN                
         BNZ   ERRINV              CLEAR MKTS/$DEL ONLY!                        
*                                                                               
VR12     XC    ELEM,ELEM                                                        
         MVI   ELEM,X'05'                                                       
         MVI   ELEM+1,13                                                        
         LA    R6,ELEM                                                          
         USING SDEFEL05,R6                                                      
*                                                                               
         LA    R4,8(R2)                                                         
         LLC   R5,5(R2)                 FIELD LENGTH                            
*                                                                               
VR13     CLI   0(R4),C'*'          IF PRECEED BY * CLT EXCP MKT                 
         BNE   VR14                                                             
         OI    SDEFCEX,X'80'                                                    
         LA    R4,1(R4)                                                         
         BCTR  R5,0                     UPDATING # BYTES VALIDATED              
VR14     LTR   R5,R5                                                            
         BZ    ERRINV                   CAN'T JUST ENTER '*'                    
         LR    RE,R5                                                            
         LR    RF,R4                                                            
         SR    R1,R1               COUNTS DIGITS IN NUMBER FOR EXPACK           
         XC    SDEFBKTY,SDEFBKTY                                                
*                                                                               
VR15     CLI   0(R4),C'='          END OF NUMBER                                
         BNE   VR16                                                             
         CR    R5,RE               IS 1ST CHAR THE '=' SIGN                     
         BE    ERRINV              YES, MKT #MISSING                            
         B     VR17                                                             
*                                                                               
VR16     CLI   0(R4),C'0'          CHECK FOR NUMERIC MKT #                      
         BL    ERRINV                                                           
         CLI   0(R4),C'9'                                                       
         BH    ERRINV                                                           
         LA    R4,1(R4)            NEXT POSN IN FLD                             
         LA    R1,1(R1)            BUMP DIGIT COUNTER                           
         BCT   R5,VR15                                                          
*                                                                               
VR17     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R0,DUB                                                           
         STH   R0,SDEFAMKT                                                      
*                                                                               
         LA    R0,ADMKEND               PAST END OF TABLE?                      
         CR    R3,R0                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'SDEFAMKT,R3),SDEFAMKT    STORE INPUT MKT IN TABLE            
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPFLD+8(0),0(RF)                                               
*                                                                               
         LR    R5,RE               R3 SAVES LENGTH OF MKT FLD                   
         LA    R1,1(R1)            REAL LENGTH OF MKT #                         
*                                                                               
         CLC   SVSTABMK,SDEFAMKT   COMPARE SAVED STATION MKT                    
         BE    ERRSTASP            CAN'T SPILL TO ITSELF                        
         BAS   RE,ISDUPMKT                                                      
         CLI   SVAPROF+7,C'C'                                                   
         BNE   *+12                                                             
         BAS   RE,CKHEIRCY              CHECK REC HEIRARCHY (CANADA)            
         BNE   SPERREX                                                          
*                                                                               
         ST    R2,FULL                  STORE ADDRESS OF MKT FLDHDR             
         LA    R2,TEMPFLD                                                       
         MVI   TEMPFLD+4,X'08'          VALID NUMERIC                           
         STC   R1,TEMPFLD+5             LENGTH OF MKT INPUT                     
         MVI   USEIONUM,2                                                       
         OI    TRNSTAT,NOVALERR         DUMMY FLD SO WE HANDLE ERRORS           
         GOTO1 VALIMKT                                                          
         NI    TRNSTAT,X'FF'-NOVALERR                                           
         MVC   AIO,AIO1                                                         
         L     R2,FULL                  RESTORE ADDRESS OF MKT FIELD            
         TM    TRNSTAT,BASERR                                                   
         BNZ   VSFMERR                                                          
         MVC   SDEFRMKT,MKTRS                                                   
*                                                                               
         L     RF,AIO2             ESTABLISH MARKET RECORD                      
         USING MKTRECD,RF                                                       
         MVC   SDEFALPH,MKTALST    SAVE MARKET ALPHA CODE                       
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VR18                NO                                           
         CLI   QMED,C'T'           TV MARKET?                                   
         BNE   VR18                NO                                           
         CLI   MKTRSVC,C'0'        NSI?                                         
         BE    VR17A               YES                                          
         CLI   MKTRSVC,C'1'        BBM?                                         
         BNE   VR18                NO, DO NOT SAVE BAD DATA!                    
*                                                                               
VR17A    MVC   SDEFRSVC,MKTRSVC    RATING SERVICE                               
         MVI   SDEF5LEN,SDEFNLEN   NEW LENGTH WITH RATING SERVICE               
         DROP  RF                                                               
*                                                                               
VR18     CLI   QMED,C'T'           IF TV MARKET                                 
         BNE   *+14                                                             
         OC    MKTRS,MKTRS            MKT MUST HAVE RATING SERVICE NO.          
*        BZ    ERRNORSM                                                         
         BZ    VRIDTVX                                                          
*                                                                               
         OC    MKTRS,MKTRS         IF MARKET HAS RATING SERVICE NO.             
         BZ    VRIDTVN                                                          
*                                                                               
         CLC   SDEFALPH,SPACES        IF NO ALPHA ID AVAILABLE                  
         BH    VRIDTVX                                                          
*                                                                               
         BRAS  RE,TRMKT                  TRANSLATE NUMERIC TO ALPHA ID          
*                                           RESULT WINDS UP IN SDEFALPH         
VRIDTVX  DS    0H                                                               
*                                                                               
         B     VRIDX                                                            
*                                                                               
VRIDTVN  DS    0H                  RADIO REQUIRES ALPHA MKT ID                  
*                                                                               
         CLC   SDEFALPH,SPACES                                                  
         BNH   ERRNOALF                                                         
*                                                                               
         OC    MKTRS,MKTRS         IF MARKET HAS NO RATING SERVICE NO.          
         BNZ   VRIDRX                                                           
*                                                                               
         BRAS  RE,TRMKT               TRANSLATE ALPHA TO NUMERIC ID             
*                                        RESULT WINDS UP IN SDEFRMKT            
VRIDRX   DS    0H                                                               
*                                                                               
VRIDX    DS    0H                                                               
*                                                                               
         CR    R1,R5               IS THERE A BKTYPE DEFINED?                   
         BNL   VR20                NO, CHECK OFFSET                             
         LA    R1,2(R1)                                                         
         CR    R1,R5                                                            
         BNE   ERRINV              INVALID BOOK TYPE                            
         CLI   1(R4),C'A'          ALPHA BOOK TYPE                              
         BL    ERRINV                                                           
         CLI   1(R4),C'Z'                                                       
         BH    ERRINV                                                           
         MVC   SDEFBKTY,1(R4)      MOVE IN BOOK TYPE                            
         XC    SDEFRMKT,SDEFRMKT                                                
*                                                                               
VR20     DS    0H                                                               
         BAS   RE,NXTSCRF               BUMP TO MKT NAME FIELD                  
         BAS   RE,NXTSCRF               BUMP TO ALPHA  FIELD                    
         BAS   RE,NXTSCRF               BUMP TO OFFSET FIELD                    
         CLI   5(R2),0                                                          
         BE    VR25                                                             
*                                                                               
*** CODE FOR OFFSET                                                             
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R5)                                          
         CLI   DMCB,0                                                           
         BNE   ERRINV                                                           
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'21000'          3.5 HOURS MAX                              
         BH    ERRINV                                                           
         C     R5,=F'-21000'                                                    
         BL    ERRINV                                                           
         CVD   R5,DUB                                                           
         DP    DUB,=P'1500'                                                     
         CP    DUB+5(3),=P'0'      MUST BE 15 MIN MULTIPLES                     
         BNE   ERRINV                                                           
         M     R4,=F'1'                                                         
         D     R4,=F'100'                                                       
         STH   R5,SDEFOSET                                                      
*                                                                               
VR25     GOTO1 ADDELEM                                                          
         B     VR35                                                             
*                                                                               
VR30     BAS   RE,NXTSCRF               BUMP TO MKT NAME FIELD                  
         BAS   RE,NXTSCRF               BUMP TO ALPHA                           
         BAS   RE,NXTSCRF               BUMP TO OFFSET FIELD                    
VR35     BAS   RE,NXTSCRF               BUMP TO DEMAND MKT                      
         BAS   RE,NXTSCRF               BUMP TO NEXT MKT                        
         LA    R3,L'ADDMKTS(R3)                                                 
         LA    R1,SPMENDH                                                       
         CR    R2,R1                                                            
         BL    VR11                                                             
*                                                                               
VRX      CLI   ACTEQU,ACTADD       ACTION = ADD?                                
         BNE   VRX1                NOPE                                         
         LA    R2,SPMMK1H          FIRST MARKET                                 
         L     R6,AIO                                                           
         LH    R1,DATADISP         DISPLACEMENT TO FIRST ELEMENT                
         AR    R6,R1               POINT TO FIRST ELEMENT                       
         CLI   0(R6),0             ANYTHING THERE?                              
         BE    ERRMIS              NO, MUST HAVE SOMETHING TO ADD!              
         B     VRXX                                                             
*                                                                               
VRX1     CLI   SVAPROF+7,C'C'                                                   
         BNE   VRXX                                                             
         BAS   RE,CKAGYDEL         CHECK AGY LVL DELETIONS OK (CANADA)          
         BNE   SPERREX                                                          
         MVC   KEY,MYKEY                                                        
*                                                                               
         OC    LOCALIO,LOCALIO     CANADA MAY HAVE DONE SOME I/O                
         BZ    VRXX                                                             
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESTORE ORIGINAL SEQUENCE                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC             RESET CORRECT DMWORK+4 D/A FOR PUTREC         
         MVC   AIO,AIO1                                                         
*                                                                               
VRXX     B     DR                  REDISPLAY RECORD                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DREC     NTR1                                                                   
*                                                                               
DR       BAS   RE,CLRSCR                                                        
*                                                                               
         LA    R2,SPMMK1H                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR100                                                            
         B     DR11                                                             
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
         BNE   DR100                                                            
DR11     LA    R4,8(R2)                 POINT TO DISPLAY AREA                   
         LR    R3,R2                    SAVE R2'S POSITION                      
         USING SDEFEL05,R6                                                      
         TM    SDEFCEX,X'80'       SKIP IF NOT CLIENT EXCEPTION                 
         BNO   DR12                                                             
         MVI   0(R4),C'*'                                                       
         LA    R4,1(R4)                 POINT TO NEXT DISPLAY BYTE              
*                                                                               
DR12     EDIT  SDEFAMKT,(4,0(R4)),0,FILL=0                                      
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD(8),=X'0000000008040000'                                  
*                                                                               
* WHEN EDIT WAS ALIGN=LEFT INSTEAD OF FILL=0                                    
*        MVC   TEMPFLD(8),=X'0000000008000000'                                  
*        STC   R0,TEMPFLD+5             INSERT LENGTH OF OUTPUT                 
*                                                                               
         MVC   TEMPFLD+8(4),0(R4)       LENGTH IN HEADER PREVAILS               
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMKT                  GET MKT NAME                            
         MVC   AIO,AIO1                                                         
*                                                                               
         LLC   R0,TEMPFLD+5                                                     
         AR    R4,R0                    ADD # OF PRINTED CHAR'S                 
         OC    SDEFBKTY,SDEFBKTY                                                
         BZ    DR14                                                             
         MVI   0(R4),C'='                                                       
         MVC   1(1,R4),SDEFBKTY                                                 
DR14     LR    R2,R3                    RESTORE R2                              
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NXTSCRF               TO MKTNAME FIELD                        
         MVC   8(L'SPMMKN1,R2),MKTNM                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NXTSCRF               TO ALPHA ID                             
*                                                                               
         CLI   SDEFEL05+1,SDEFALPH-SDEFEL05 SKIP IF FIELD NOT THERE             
         BNH   DR15                                                             
         MVC   8(3,R2),SDEFALPH         MKT ALPHA ID                            
         CLI   SVAPROF+7,C'C'           CANADIAN AGENCY?                        
         BNE   DR15                     NO                                      
         CLI   QMED,C'T'                TV MARKET?                              
         BNE   DR15                     NO                                      
         OC    8(3,R2),8(R2)            HAVE AN ALPHA MKT?                      
         BNZ   DR15                     YES                                     
         MVC   8(3,R2),=C'***'          NO - MOVE IN ASTERISKS                  
*                                                                               
DR15     BAS   RE,NXTSCRF               TO OFFSET                               
         EDIT  SDEFOSET,(4,8(R2)),0,FLOAT=-,ZERO=BLANK                          
*                                                                               
         BAS   RE,NXTSCRF               TO RATSVC FIELD                         
*                                                                               
         OC    SDEFBKTY,SDEFBKTY                                                
         BNZ   DR20                                                             
*                                                                               
         OC    SDEFRMKT,SDEFRMKT   SKIP IF NO RATING SERVICE MARKET             
         BZ    DR20                                                             
*                                                                               
         EDIT  SDEFRMKT,(4,8(R2)),0,FILL=0                                      
         MVC   WORK(2),SDEFRMKT                                                 
         LR    R5,R2                                                            
         BAS   RE,FNDMKT                                                        
*                                                                               
DR20     CLI   SVAPROF+7,C'C'           CANADIAN AGENCY?                        
         BNE   DR25                     NO                                      
         CLI   QMED,C'T'                TV MARKET?                              
         BNE   DR25                     NO                                      
         CLC   8(10,R2),SPACES          HAVE RATING SVC INFO?                   
         BH    DR25                     YES                                     
         MVC   8(16,R2),=C'** NO RTG SVC **'                                    
*                                                                               
DR25     BAS   RE,NXTSCRF               TO NEXT MKT FIELD                       
         LA    R0,SPMENDH                                                       
         CR    R2,R0                                                            
         BE    DR100                                                            
         B     DR10                                                             
*                                                                               
* NEED TO ACCOUNT FOR LIST CHANGE                                               
DR100    CLI   ACTEQU,ACTSEL                                                    
         BNE   DR100A                                                           
         CLI   SAVESEL,C'C'                                                     
         BE    DR100B                                                           
DR100A   CLI   ACTEQU,ACTCHA                                                    
         BNE   DRX                                                              
DR100B   XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
DRX      MVC   AIO,AIO1                                                         
         MVI   ACTELOPT,C'N'            DO NOT ADD ACTIVITY ELEMENTS            
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING SDEFRECD,R6                                                      
*                                                                               
         CLI   SVAPROF+7,C'C'           CANADIAN?                               
         BNE   DK20                     NO CHECK US                             
         MVC   SPMRAT(3),=C'BBM'                                                
         MVI   BKVALSRC,C'B'                                                    
         CLI   SDEFKRSV,C'1'                                                    
         BE    DK45                                                             
         MVC   SPMRAT(3),=C'CSI'                                                
         MVI   BKVALSRC,C'C'                                                    
         B     DK45                                                             
*                                                                               
DK20     MVC   SPMRAT(3),=C'ARB'                                                
         MVI   BKVALSRC,C'A'                                                    
         CLI   SDEFKRSV,C'1'                                                    
         BE    DK45                                                             
         MVC   SPMRAT(3),=C'NSI'                                                
         MVI   BKVALSRC,C'N'                                                    
*                                                                               
DK45     OI    SPMRATH+6,X'80'                                                  
*                                                                               
         XC    SPMSTA,SPMSTA       INIT STATION FIELD                           
*                                                                               
         MVC   SPMSTA(4),SDEFKSTA                                               
*                                                                               
         CLI   SDEFKSTA+4,C' '     IF NOT TV                                    
         BNH   DK46                                                             
*                                                                               
         LA    RF,SPMSTA+3            FIND NEXT AVAILABLE PRINT SPACE           
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         AHI   RF,1                                                             
*                                                                               
         MVI   0(RF),C'-'             PRINT BAND                                
         MVC   1(1,RF),SDEFKSTA+4                                               
*                                                                               
DK46     DS    0H                                                               
*                                                                               
         OI    SPMSTAH+6,X'80'                                                  
*                                                                               
         LA    R2,SPMSTAH                                                       
         MVI   USEIONUM,2                                                       
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DK48                                                             
         CLI   THISLSEL,C'C'       ...TIDYING RECORDS FOR USER                  
         BNE   DK48                                                             
         OI    TRNSTAT,NOVALERR    ...STATION REC MAY NOT EXIST                 
DK48     MVI   STAVERR,0                                                        
         GOTO1 VALISTA                                                          
         MVC   STAVERR,TRNSTAT                                                  
         NI    TRNSTAT,255-(NOVALERR+BASERR)                                    
         MVC   SVSTABMK,BMKT            SAVE STATION BINARY MARKET              
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   SPMCLT,SPACES            CLEAR CLIENT FIELD                      
         MVI   SPMCLTH+5,0                                                      
         OI    SPMCLTH+6,X'80'                                                  
         OC    SDEFKCLT,SDEFKCLT                                                
         BZ    DK50                                                             
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,SDEFKCLT),SPMCLT                          
         OI    SPMCLTH+6,X'80'                                                  
*                                                                               
DK50     MVC   SAVEKEY,0(R6)                                                    
*                                                                               
DKX      CLI   ACTEQU,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SAVESEL,THISLSEL                                                 
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
DELREC   BAS   RE,DREC             DISPLAY THE RECORD FIRST!                    
         L     R6,AIO              A(SPILLDEF RECORD)                           
         MVI   ELCODE,X'05'        LOOK FOR MARKET ELEMENTS                     
         BAS   RE,GETEL            ANY MARKETS?                                 
         BE    DELMKT              YES - ERROR                                  
*                                                                               
         USING SDEFRECD,R6                                                      
         L     R6,AIO              A(SPILLDEF RECORD)                           
         OC    SDEFKCLT,SDEFKCLT   CLIENT SPECIFIC SPILLDEF?                    
         BNZ   DELX                YES - O.K. TO DELETE                         
         DROP  R6                                                               
*                                                                               
         NI    DMINBTS,X'F7'       DON'T WANT DELETED RECS!                     
*                                                                               
         GOTO1 SEQ                 READ RECORD AFTER ONE WANT TO DEL            
*                                                                               
         OI    DMINBTS,X'08'       RESTORE PASSING OF DELETED RECS              
         CLC   KEY(SDEFKCLT-SDEFRECD),KEYSAVE                                   
         BE    DELCLT              CLIENT SPECIFIC RECORD STILL EXISTS          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         GOTO1 HIGH                MAKE SURE WE DELETE THE RIGHT RECORD         
*                                                                               
DELX     B     XIT                                                              
*                                                                               
         USING SDEFRECD,R6                                                      
RESTREC  L     R6,AIO              A(SPILLDEF RECORD TO RESTORE)                
         OC    SDEFKCLT,SDEFKCLT   CLIENT SPECIFIC SPILLDEF?                    
         BZ    RESX                NO - O.K. TO DELETE                          
*                                                                               
         MVC   SAVEKEY,KEY         SAVE OFF THE KEY                             
         LA    R6,KEY                                                           
         XC    SDEFKCLT,SDEFKCLT   CLEAR THE CLIENT                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     FOUND IT?                                    
         BE    *+6                 YES                                          
         DC    H'0'                NO - HOW IS THIS NOT ON THE FILE?            
*                                                                               
         TM    KEY+13,X'80'        DELETED?                                     
         BNZ   RESERR              YES - NEED TO RESTORE FIRST!                 
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         GOTO1 HIGH                MAKE SURE WE RESTORE THE RIGHT REC           
*                                                                               
RESX     B     XIT                                                              
*                                                                               
* ONLINE LIST                                                                   
*                                                                               
LR       DS    0H                                                               
         USING SDEFRECD,R4                                                      
         LA    R4,KEY                                                           
         OC    KEY,KEY        IS THIS FIRST TIME AT LIST SCREEN?                
         BNZ   LR10           NO, DO NOT BUILD KEY                              
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH               FIND FIRST DIRECTORY REC                      
         B     LR25                                                             
*                                                                               
LR20     GOTO1 SEQ                FIND SUBSEQUENT DIRECTORY RECS                
LR25     CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     DS    0H                                                               
         CLC   KEY(4),SAVEKEY      IF PAST SPILL RECS OF THIS AGY               
         BNE   LRX                STOP READING RECORDS                          
*                                                                               
* FILTERS                                                                       
*                                                                               
*        MEDIA                                                                  
*                                                                               
         OC    SVMED,SVMED         IF WE HAVE A MEDIA FILTER                    
         BZ    LRMEDX                                                           
*                                                                               
         CLI   SVMED,C'R'             IF MEDIA NOT RADIO                        
         BE    LRMEDRD                                                          
*                                                                               
         CLI   SDEFKSTA+4,0              STATION MUST HAVE NO MEDIA             
         BE    LRMEDX                                                           
*                                                                               
         B     LR60                      ELSE SKIP                              
*                                                                               
LRMEDRD  DS    0H                                                               
*                                                                               
         CLI   SDEFKSTA+4,C'A'           STATION MUST HAVE MEDIA C'A'           
         BE    *+8                       OR                                     
         CLI   SDEFKSTA+4,C'F'           STATION MUST HAVE MEDIA C'F'           
         BE    LRMEDX                                                           
*                                                                               
         B     LR60                      ELSE SKIP                              
*                                                                               
LRMEDX   DS    0H                                                               
*                                                                               
         OC    SAVECLT,SAVECLT          CLIENT FILTER?                          
         BZ    LR30A                    NO CLIENT FILTER                        
*                                                                               
         CLC   SDEFKCLT,BCLT                                                    
         BNE   LR20                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
LR30A    GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVLMKT,SVLMKT                                                    
         BZ    LR32                                                             
*                                                                               
         MVI   ELCODE,X'05'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR31     BAS   RE,NEXTEL                                                        
         BNE   LR20                                                             
*                                                                               
         USING SDEFEL05,R6                                                      
         CLC   SDEFAMKT,SVLMKT                                                  
         BNE   LR31                                                             
         DROP  R6                                                               
* LOOP HERE TO BUMP THROUGH X'05' ELEMENTS LOOKING FOR MKT                      
*                                                                               
LR32     MVC   LISTAR,SPACES      CLEAR PRINT LINE OF LIST ENTRIES              
         L     R6,AIO                                                           
         USING SDEFRECD,R6                                                      
*                                                                               
LR32A    DS    0H                                                               
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
         USING LISTD,R2                                                         
*                                                                               
         CLI   SVAPROF+7,C'C'           CANADIAN?                               
         BNE   LR33                     NO CHECK US                             
         MVC   LSRTSV,=C'BBM'                                                   
         CLI   SDEFKRSV,C'1'                                                    
         BE    LR35                                                             
         MVC   LSRTSV,=C'CSI'                                                   
         B     LR35                                                             
*                                                                               
LR33     MVC   LSRTSV,=C'ARB'                                                   
         CLI   SDEFKRSV,C'1'                                                    
         BE    LR35                                                             
         MVC   LSRTSV,=C'NSI'                                                   
*                                                                               
LR35     DS    0H                                                               
         MVC   LSSTA,SDEFKSTA                                                   
         OC    SDEFKCLT,SDEFKCLT                                                
         BZ    LR40                                                             
         CLI   SPLCLTH+5,0                                                      
         BNE   LR39                                                             
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         LA    R4,KEY              SET UP THE KEY                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(L'BAGYMD),BAGYMD                                           
         MVC   KEY+2(L'BCLT),SDEFKCLT                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                RETURN 'NO' IF ERROR                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         L     R5,AIO3                                                          
         MVC   SVCPROF,CPROF-CLTHDRD(R5)                                        
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                RETURN 'NO' IF ERROR                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DMDSKADD,KEY+14     D/A OF SPILLDEF RECORD FOR LISTAR            
*                                                                               
LR39     GOTO1 CLUNPK,DMCB,(SVCPROF+6,SDEFKCLT),LSCLT                           
         DROP  R6                                                               
*                                                                               
LR40     SR    R5,R5               SHOW MARKETS                                 
         L     R6,AIO                                                           
         USING SDEFEL05,R6                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR41     BAS   RE,NEXTEL                                                        
         BNE   LR43                                                             
         AHI   R5,1                TOTAL MKTS                                   
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         CHI   R5,LPMKTMXQ                                                      
         B     *+8                                                              
         CHI   R5,LSMKTMXQ                                                      
         BH    LR41                (SHOWN AS MANY MKTS AS POSSIBLE)             
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         MHI   R1,L'LSMKT                                                       
         LA    R1,LSMKT(R1)                                                     
         TM    SDEFCEX,X'80'                                                    
         BNO   *+8                                                              
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         EDIT  SDEFAMKT,(4,0(R1)),0,FILL=0                                      
         B     LR41                                                             
*                                                                               
LR43     LA    R1,LSMORE                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR44                                                             
         LA    R1,LPMORE                                                        
         SHI   R5,LPMKTMXQ                                                      
         B     *+8                                                              
LR44     SHI   R5,LSMKTMXQ                                                      
         LTR   R5,R5                                                            
         BNP   LR45                                                             
         MVI   0(R1),C'+'                                                       
         EDIT  (R5),(2,1(R1)),0,ALIGN=LEFT                                      
         DROP  R2                                                               
*                                                                               
LR45     CLI   MODE,LISTRECS                                                    
         BNE   LR50                                                             
         GOTO1 LISTMON                                                          
         B     LR60                                                             
*                                                                               
LR50     GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR60     B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* XRECADD AND XRECPUT ROUTINE                                                   
*                                                                               
REQREC   DS    0H                                                               
         USING SDEFRECD,R5                                                      
         LA    R5,SAVEKEY                                                       
         L     R3,AIO2                                                          
         XC    0(110,R3),0(R3)                                                  
         MVI   10(R3),44                                                        
         MVI   14(R3),106                                                       
         MVI   26(R3),X'40'                                                     
         MVC   27(79,R3),26(R3)                                                 
         MVC   26(2,R3),=C'44'                                                  
         MVC   28(2,R3),AGENCY                                                  
         MVC   30(1,R3),QMED                                                    
         MVC   31(3,R3),=C'ALL'                                                 
         OC    SDEFKCLT,SDEFKCLT                                                
         BZ    *+10                                                             
         MVC   31(3,R3),QCLT                                                    
         MVC   40(3,R3),=C'NSI'                                                 
         CLI   SDEFKRSV,C'0'                                                    
         BE    *+10                                                             
         MVC   40(3,R3),=C'BBM'                                                 
         MVC   44(5,R3),SDEFKSTA                                                
         DROP  R5                                                               
*                                                                               
         MVI   87(R3),C'L'                                                      
         MVC   94(7,R3),=C'CONTROL'                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO2,AIO2                     
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SAVEKEY                                                      
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         B     DR                                                               
*                                                                               
RQRX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
ERRNOALF MVC   ERRNUM,=AL2(NOALFA)                                              
         B     SPERREX                                                          
ERRNORSM MVC   ERRNUM,=AL2(NORSMK)                                              
         B     SPERREX                                                          
ERRSTASP MVC   ERRNUM,=AL2(STASPL)                                              
         B     SPERREX                                                          
ERRDUPMK MVC   ERRNUM,=AL2(DUPMK)                                               
         B     SPERREX                                                          
ERRNOAGY MVC   ERRNUM,=AL2(NOAGYSPL)                                            
         B     SPERREX                                                          
ERRINCLI MVC   ERRNUM,=AL2(860)                                                 
         B     SPERREX                                                          
ERCDNCBL MVC   ERRNUM,=AL2(841)    CANADIAN CABLE CANNOT HAVE SPILL             
         B     SPERREX                                                          
DELMKT   MVC   ERRNUM,=AL2(1350)   MUST REMOVE MKTS BEFORE DELETING             
         B     SPERREX                                                          
DELCLT   MVC   ERRNUM,=AL2(408)    DELETE STATION AT CLIENT LEVEL FIRST         
         B     SPERREX                                                          
RESERR   MVC   ERRNUM,=AL2(1351)   RETORE THE STATION LEVEL REC FIRST           
         B     SPERREX                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
NORSMK   EQU   249                                                              
NOALFA   EQU   788                                                              
STASPL   EQU   459                                                              
DUPMK    EQU   439                                                              
NOAGYSPL EQU   141                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
**** ROUTINE TO BUMP TO NEXT SCREEN FIELD ****                                  
NXTSCRF  DS    0H                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
***********************************************************************         
*        CLEARS BOTTOM OF SCREEN                                                
***********************************************************************         
*                                                                               
CLRSCR   NTR1                                                                   
         LA    R2,SPMMK1H                                                       
         LA    R3,SPMENDH                                                       
*                                                                               
CLRSCR10 LLC   R1,0(R2)            FIELD LENGTH                                 
         SHI   R1,9                8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               PAST LAST OUTPUT FIELD?                      
         BNH   CLRSCR10            NO, CONTINUE                                 
         B     XIT                      YES, DONE                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* FNDMKT  -    FIND MKT NAME IN DEMAND                                          
*              R5       = FLDHDR OF RTG SVC MKT FIELD                           
**********************************************************************          
*                                                                               
FNDMKT   NTR1                                                                   
         USING DBLOCKD,R3                                                       
         LA    R3,DBLOCK1                                                       
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELRMK,WORK          SAVE THE RATING SRVC MKT #                
*                                                                               
         L     R1,AIO2                                                          
         ST    R1,DBAREC                                                        
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,QMED                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
***                                                                             
* SOFT DEMOS SUPPORT - CHECK RECORD FOR RATING SERVICE, NOT KEY                 
***                                                                             
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   FNDMK05             NO                                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   FNDMK05             NO                                           
         USING SDEFEL05,R6                                                      
         CLI   SDEF5LEN,SDEFNLEN   HAVE RATING SERVICE?                         
         BL    FNDMK05             NO, GET IT FROM KEY                          
*                                                                               
         MVC   13(4,R5),=C'BBM '                                                
         CLI   SDEFRSVC,C'1'       BBM?                                         
         BE    *+10                YES                                          
         MVC   13(4,R5),=C'NSI '                                                
         LA    R5,4(R5)                                                         
*                                                                               
FNDMK04  MVI   DBSELSRC,C'A'       USE BBM                                      
         CLI   SDEFRSVC,C'1'       BBM?                                         
         BE    FNDMK10             YES                                          
         MVI   DBSELSRC,C'N'       NSI                                          
         B     FNDMK10                                                          
         DROP  R6                                                               
*                                                                               
         USING SDEFRECD,R6                                                      
FNDMK05  L     R6,AIO1                                                          
*                                                                               
         MVI   DBSELSRC,C'N'       NSI                                          
*                                                                               
         CLI   SDEFKRSV,C'1'                                                    
         BNE   *+8                                                              
         MVI   DBSELSRC,C'A'       ARB                                          
*                                                                               
FNDMK10  MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,SDEFKAGY                                                
         MVC   DBSELSTA,SDEFKSTA                                                
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
*                                                                               
         MVC   WORK2,SPACES                                                     
         L     RF,DBAREC                                                        
         USING DMKEY,RF                                                         
*                                                                               
         LA    RF,DMFRSTEL                                                      
*                                                                               
         DROP  RF                                                               
         USING DMELEM,RF                                                        
*                                                                               
         LLC   R1,DMLEN                                                         
         SHI   R1,5                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8+5(0,R5),DMMNAME     GET MKT NAME                               
         DROP  RF,R3,R6                                                         
*                                                                               
FNDMKTX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ISDUPMKT - HAS MKT INPUT ALREADY BEEN INPUT ?                                 
**********************************************************************          
*                                                                               
ISDUPMKT NTR1                                                                   
         USING SDEFEL05,R5                                                      
         LA    R5,ELEM                                                          
*                                                                               
         LA    R1,ADDMKTS                                                       
DM10     CR    R1,R3                                                            
         BE    DMX                                                              
         CLC   SDEFAMKT,0(R1)                                                   
         BE    ERRDUPMK                                                         
         LA    R1,L'ADDMKTS(R1)                                                 
         LA    R0,ADMKEND                                                       
         CR    R1,R0                                                            
         BL    DM10                                                             
         DC    H'0'                                                             
DMX      XIT                                                                    
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK NOT ADDING SPILL FOR CANADIAN CABLE                           *         
* USES IO2                                                            *         
* EXIT  - CC NOT EQUAL IF ERROR                                       *         
***********************************************************************         
         SPACE                                                                  
CKCDNCBL NTR1  ,                                                                
         LA    R6,KEY                                                           
         USING NDEFRECD,R6                                                      
         XC    KEY,KEY                                                          
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGENCY                                                  
         MVC   NDEFKNET,QSTA                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(NDEFKCLT-NDEFKEY),KEYSAVE                                    
         BNE   CHKCCOKX                                                         
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,NDEFNELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   CHKCCERX       SHOULD BE ONE! STOP ADD BETTER THAN DUMP          
         CLI   NDEFNET-NDEFEL02(R6),NDEFCABQ                                    
         BNE   CHKCCOKX                                                         
CHKCCERX LTR   RB,RB                                                            
         B     *+6                                                              
CHKCCOKX CR    RB,RB                                                            
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* GET A COPY OF AGENCY LEVEL SPILL RECORD FOR CROSS-CHECKING          *         
* EXIT  - AGENCY LEVEL SPILL RECORD IN IO3                            *         
*         LOCALIO=Y IF I/O DONE                                       *         
*         CC NOT EQUAL IF ERROR                                       *         
***********************************************************************         
         SPACE                                                                  
RDAGYSPL NTR1 ,                    *** CANADA ONLY ***                          
         MVC   SVADDR,DMDSKADD                                                  
         MVC   AIO,AIO3                                                         
*                                                                               
PRIMEK   USING SDEFRECD,MYKEY                                                   
         OC    PRIMEK.SDEFKCLT(L'SDEFKCLT),PRIMEK.SDEFKCLT                      
         BNZ   RDAGY10             GET AGY LEVEL SPILL REC                      
         DROP  PRIMEK                                                           
*                                  DEALING WITH AGY LEVEL REC                   
         CLI   ACTNUM,ACTADD                                                    
         BE    RDAGYOKX            ADDING ONE SO NOTHING TO GET                 
         L     R2,AIO1                                                          
         L     R4,AIO3                                                          
         SR    R3,R3                                                            
         ICM   R3,3,13(R2)                                                      
         LR    R5,R3                                                            
         MVCL  R4,R2               COPY IT                                      
         B     RDAGYOKX                                                         
*                                                                               
RDAGY10  MVC   KEY,MYKEY           ENSURE USING 'ORIGINAL' KEY                  
         XC    KEY+SDEFKCLT-SDEFRECD(L'SDEFKCLT),KEY+SDEFKCLT-SDEFRECD          
         MVI   LOCALIO,C'Y'                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RDAGYERX            NO AGY LEVEL REC!!!                          
         GOTO1 GETREC                                                           
*                                                                               
RDAGYOKX CR    RB,RB                                                            
         B     *+6                                                              
RDAGYERX LTR   RB,RB                                                            
RDAGYX   MVC   AIO,AIO1            RESTORE                                      
         MVC   DMDSKADD,SVADDR                                                  
         MVC   KEY,MYKEY                                                        
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* CHECK HEIRARCHY - CLIENT MARKETS MUST ALSO EXIST AT AGENCY LEVEL    *         
* ENTRY - R6=A(SDEFEL05 ELEM)                                         *         
*         IO3=AGENCY LEVEL SPILL RECORD                               *         
* EXIT  - CC NOT EQUAL AND ERRNUM SET ON ERROR                        *         
***********************************************************************         
         SPACE                                                                  
CKHEIRCY NTR1  ,                   *** CANADA ONLY ***                          
PRIMEK   USING SDEFRECD,MYKEY                                                   
         OC    PRIMEK.SDEFKCLT(L'SDEFKCLT),PRIMEK.SDEFKCLT                      
         BZ    CHKHOKX             DEALING WITH AGENCY LEVEL REC                
         DROP  PRIMEK                                                           
*                                                                               
         USING SDEFEL05,R6                                                      
         LR    R3,R6               REMEMBER ELEM ENTERED WITH                   
         L     R6,AIO3                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHKHNXT  BAS   RE,NEXTEL                                                        
         BNE   CHKHERX                                                          
         CLC   SDEFAMKT,SDEFAMKT-SDEFEL05(R3)                                   
         BNE   CHKHNXT                                                          
*                                                                               
CHKHOKX  CR    RB,RB                                                            
         B     CHKHX                                                            
CHKHERX  MVC   ERRNUM,=AL2(1184)   (ADD TO AGY LEVEL)                           
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTLTXT,4                                                         
         LA    R1,FULL                                                          
         STCM  R1,7,GTATXT                                                      
         LR    R6,R3                                                            
         EDIT  SDEFAMKT,(4,FULL),0,FILL=0                                       
         DROP  RF                                                               
         LTR   RB,RB                                                            
CHKHX    XIT1  ,                   EXIT WITH CC SET                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK DELETED AGENCY MARKETS NOT EXIST AT CLIENT LEVEL              *         
* USES I02                                                            *         
* ENTRY - IO3=AGENCY LEVEL SPILL RECORD                               *         
* EXIT  - LOCALIO=Y IF I/O DONE                                       *         
*         NOT EQUAL WITH ERRNUM/CURSOR (R2) SET - IF ERROR            *         
***********************************************************************         
         SPACE                                                                  
CKAGYDEL NTR1  ,                   *** CANADA ONLY ***                          
         MVC   SVADDR,DMDSKADD                                                  
         MVC   AIO,AIO2                                                         
*                                                                               
PRIMEK   USING SDEFRECD,MYKEY                                                   
         OC    PRIMEK.SDEFKCLT(L'SDEFKCLT),PRIMEK.SDEFKCLT                      
         BNZ   CHKADOKX            DEALING WITH CLIENT LEVEL REC                
         DROP  PRIMEK                                                           
*                                                                               
         L     R6,AIO3             DETERMINE ANY MKTS DELETED                   
         USING SDEFEL05,R6                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHKAD10  BAS   RE,NEXTEL                                                        
         BNE   CHKADOKX            CHECKED ALL ORIGINAL MKTS                    
*                                                                               
         LA    R4,ADDMKTS          LOOKUP IN CURRENT MARKET LIST                
CHKAD20  CLC   SDEFAMKT,0(R4)      (REMOVED MKTS WILL BE NULL)                  
         BE    CHKAD10             TRY NEXT ORIGINAL MKT                        
         LA    R4,L'ADDMKTS(R4)                                                 
         LA    RF,ADMKEND                                                       
         CR    R4,RF                                                            
         BL    CHKAD20             TRY NEXT CURRENT MKT                         
*                                                                               
CHKAD30  MVC   KEY,MYKEY           CHECK DEL MKT NOT IN CLI LVL RECS            
         LR    R3,R6               REMEMBER ELEM IN ORIGINAL REC (IO3)          
         MVI   LOCALIO,C'Y'                                                     
         GOTO1 HIGH                (WILL READ AGY LEVEL REC, HENCE SEQ)         
CHKAD32  GOTO1 SEQ                                                              
         CLC   KEY(SDEFKCLT-SDEFRECD),KEYSAVE                                   
         BE    *+10                                                             
         LR    R6,R3               READDRESS ORIGINAL ELEM (IO3)                
         B     CHKAD10             CHECKED CLI LEVEL RECS FOR THIS MKT          
         GOTO1 GETREC                                                           
         L     R6,AIO              SEE IF MKT DEFINED                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHKAD35  BAS   RE,NEXTEL                                                        
         BNE   CHKAD32             MKT NOT IN THIS CLIENT LEVEL REC             
         CLC   SDEFAMKT,SDEFAMKT-SDEFEL05(R3)                                   
         BNE   CHKAD35                                                          
*                                  DEL MKT EXISTS AT CLIENT LEVEL               
CHKADERX MVC   ERRNUM,=AL2(1183)   (REMOVE FROM CLIENT LEVEL)                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTLTXT,4                                                         
         LA    R1,FULL                                                          
         STCM  R1,7,GTATXT                                                      
         LR    R6,R3                                                            
         EDIT  SDEFAMKT,(4,FULL),0,FILL=0                                       
         DROP  RF                                                               
         LA    R2,SPMMK1H                                                       
         LTR   RB,RB                                                            
         B     *+6                                                              
CHKADOKX CR    RB,RB                                                            
CHKADX   MVC   AIO,AIO1            RESTORE                                      
         MVC   DMDSKADD,SVADDR                                                  
         MVC   KEY,MYKEY                                                        
         XIT1  REGS=(R2)           EXIT WITH CC SET & CURSOR SET                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST        NO LIST DELETE                          
*                                                                               
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**** CONSTANTS AND TABLES                                                       
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'SPILL LIST'                                              
         SSPEC H2,30,C'----------'                                              
         SPACE 1                                                                
         SSPEC H4,1,C'RTSV'                                                     
         SSPEC H4,7,C'STA'                                                      
         SSPEC H4,14,C'CLIENT'                                                  
         SPACE 1                                                                
         SSPEC H5,1,C'----'                                                     
         SSPEC H5,7,C'---'                                                      
         SSPEC H5,14,C'------'                                                  
         DC    X'00'                                                            
         EJECT                                                                  
**********************************************************************          
* TRMKT   -    TRANSLATE BETWEEN ALPHA AND NUMERIC MARKET IDS                   
*              R5       = FLDHDR OF RTG SVC MKT FIELD                           
*              R6       = SDEFEL05  ELEMENT                                     
**********************************************************************          
*                                                                               
TRMKT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SDEFEL05,R6         ESTABLISH SPILL ELEMENT                      
*                                                                               
         USING DBLOCKD,R3                                                       
         LA    R3,DBLOCK1                                                       
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVI   DBFUNCT,DBCNVA2N    ASSUME CONVERTING ALPHA TO NUMERIC           
*                                                                               
         CLC   SDEFALPH,SPACES     IF NO ALPHA AVAILABLE                        
         BH    *+8                                                              
         MVI   DBFUNCT,DBCNVN2A       SWITCH TO NUMERIC TO ALPHA                
*                                                                               
         MVC   DBSELRMK,SDEFRMKT   PASS RTG SVC NUMERIC MARKET NUMBER           
         MVC   DBSELALF,SDEFALPH   PASS RTG SVC ALPHA   MARKET ID               
*                                                                               
         L     R1,AIO2                                                          
         ST    R1,DBAREC                                                        
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,QMED                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
*                                                                               
         USING SDEFRECD,R4                                                      
         L     R4,AIO1                                                          
*                                                                               
         MVI   DBSELSRC,C'N'       NSI                                          
*                                                                               
         CLI   SDEFKRSV,C'1'                                                    
         BNE   *+8                                                              
         MVI   DBSELSRC,C'A'       ARB                                          
***                                                                             
* SOFT DEMOS SUPPORT - CHECK RECORD FOR RATING SERVICE, NOT KEY                 
***                                                                             
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   TRMK05              NO                                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   TRMK05              NO                                           
         CLI   SDEF5LEN,SDEFNLEN   HAVE RATING SERVICE?                         
         BL    TRMK05              NO, USE KEYS RATING SERVICE                  
         MVI   DBSELSRC,C'A'       USE BBM                                      
         CLI   SDEFRSVC,C'1'       BBM?                                         
         BE    TRMK05              YES                                          
         MVI   DBSELSRC,C'N'       NSI                                          
*                                                                               
TRMK05   MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,SDEFKAGY                                                
         MVC   DBSELSTA,SDEFKSTA                                                
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         CLI   DBERROR,X'10'       NOT FOUND                                    
         BNO   TRMK10                                                           
*                                                                               
TRMK10   DS    0H                                                               
*                                                                               
         MVC   SDEFRMKT,DBSELRMK   RETURN RTG SVC NUMERIC MARKET NUMBER         
         MVC   SDEFALPH,DBSELALF   RETURN RTG SVC ALPHA   MARKET ID             
*                                                                               
TRMKTX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA5D               MAINT. SCREEN                           
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA6D               LIST SCREEN                             
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSDEF                                                      
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
CLIENT   DS    CL3                                                              
SAVEKEY  DS    CL13                                                             
MYKEY    DS    XL(L'KEY)           (FULL DIR KEY INCL CTL & D/A !!!)            
SVADDR   DS    A                                                                
ERRNUM   DS    XL2                                                              
TEMPFLD  DS    XL12                                                             
SVSTABMK DS    XL2                                                              
SVSTAT   DS    CL5                                                              
SVRSV    DS    CL1                                                              
SVLMKT   DS    XL2                      SAVED MKT FILTER                        
SAVECLT  DS    CL3                      SAVED INPUT CLIENT FILTER               
SVMED    DS    CL1                 SAVED INPUT MEDIA                            
LOCALIO  DS    XL1              FLAG (Y) IF DONE I/O DURING VALREC MODE         
STAVERR  DS    XL(L'TRNSTAT)       TRNSTAT FLAG FOR DDS CHASEL STA VALS         
*                                                                               
ADDMKTS  DS    16XL2                                                            
ADMKEND  DS    0H                                                               
ADMKLNEQ EQU   ADMKEND-ADDMKTS                                                  
*                                                                               
WORK2    DS    CL64                                                             
SAVESEL  DS    CL1                                                              
*                                                                               
DBLKLAB  DS    D                                                                
DBLOCK1  DS    XL(L'DBLOCK)                                                     
         DS    XL14                                                             
DBLOCK1X DS    0X                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
*                                                                               
**** ONLINE LIST LINE                                                           
*                                                                               
LISTD    DSECT                                                                  
LSRTSV   DS    CL3                                                              
         DS    CL3                                                              
LSSTA    DS    CL5                                                              
         DS    CL2                                                              
LSCLT    DS    CL3                                                              
         DS    CL1                                                              
LSMKT    DS    CL6                                                              
LSMKTMXQ EQU   (L'SPLLIN1-((LSMKT-LISTD)+L'LSMORE))/L'LSMKT  #MKTS FIT          
         ORG   LISTD+(L'SPLLIN1-L'LSMORE)                                       
LSMORE   DS    CL3                                                              
         ORG   LISTD+(L'P-L'LPMORE)                                             
LPMKTMXQ EQU   (L'P-((LSMKT-LISTD)+L'LPMORE))/L'LSMKT  (PLINE)                  
LPMORE   DS    CL3                                                              
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SPSFM40   03/26/10'                                      
         END                                                                    
