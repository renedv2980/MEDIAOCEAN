*          DATA SET SPRES21    AT LEVEL 020 AS OF 04/01/14                      
*PHASE T20F21A                                                                  
         TITLE 'T20F21- RESEARCH STAION LIST MAINT/LIST'                        
T20F21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F21                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING CONHEADH-64,R3                                                   
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   CTRY,SVCTRY                                                      
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQ                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R5,KEY                                                           
         USING SLKEY,R5                                                         
         XC    KEY,KEY                                                          
         MVI   SLKTYPE,X'0D'                                                    
         MVI   SLKSUB,X'5B'                                                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(12),=X'0C00000000030000D9C1D900'                            
         CLI   CTRY,C'C'                                                        
         BNE   *+10                                                             
         MVC   ELEM+8(3),=C'BBR'                                                
         LA    R2,ELEM                                                          
         GOTO1 VVALSRC             FAKE THE SOURCE FIELD TO 'RA'                
         MVC   SLKAM,BAGYMD                                                     
*                                                                               
         LA    R2,STLNAMH                                                       
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VKX                                                              
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK10     DS    0H                                                               
         GOTO1 ANY                                                              
         MVC   SLKNAME,WORK                                                     
*                                                                               
VKX      MVC   SVKEY,KEY                                                        
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       XC    ELEM,ELEM                                                        
         MVC   ELEM(12),=X'0C00000000030000D9C1D900'                            
         CLI   CTRY,C'C'                                                        
         BNE   *+10                                                             
         MVC   ELEM+8(3),=C'BBR'                                                
         LA    R2,ELEM                                                          
         GOTO1 VVALSRC             FAKE THE SOURCE FIELD TO 'RA'                
*                                                                               
         MVI   ELCODE,SLDELQ                                                    
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,STLDESCH                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R6,ELEM             BUILD DESCRIPTION ELEMENT                    
         USING SLDELEM,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   SLDELEM,SLDELQ                                                   
         MVI   SLDLEN,SLDLENEQ                                                  
         MVC   SLDDESC,SPACES      PAD FIELD                                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SLDDESC(0),8(R2)                                                 
*                                                                               
         LA    R2,STLMKTAH                                                      
         CLI   5(R2),0             ARB MKT NOT MANDITORY                        
         BNE   VR5                                                              
         LA    R2,STLMKNAH                                                      
         MVC   8(L'STLMKNA,R2),SPACES     CLEAR MKT NAME                        
         OI    6(R2),X'80'                                                      
         B     VR10                                                             
*                                                                               
VR5      BAS   RE,VALNUM                                                        
         MVC   SLDMKTA,FULL+2                                                   
         MVC   ARBI,FULL+2         SAVE ARB CODE                                
         MVI   DBSELSRC,C'A'                                                    
         CLI   CTRY,C'C'                                                        
         BNE   *+8                                                              
         MVI   DBSELSRC,C'M'                                                    
         BAS   RE,VALMKT           READ FOR MARKET NAME                         
         LA    R2,STLMKNAH                                                      
         MVC   8(L'STLMKNA,R2),WORK+2        MOVE ARB NAME TO SCREEN            
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
VR10     LA    R2,STLMKTBH                                                      
         CLI   5(R2),0             BIR MKT NOT NECC                             
         BNE   VR11                                                             
         LA    R2,STLMKNBH                                                      
         MVC   8(L'STLMKNB,R2),SPACES     CLEAR MKT NAME                        
         OI    6(R2),X'80'                                                      
         B     VR12                                                             
*                                                                               
VR11     BAS   RE,VALNUM                                                        
         MVC   SLDMKTB,FULL+2                                                   
         MVC   BIR,FULL+2          SAVE BIRCH CODE                              
         BAS   RE,VALMKT           READ FOR MKT NAME                            
         LA    R2,STLMKNBH                                                      
         MVC   8(L'STLMKNB,R2),WORK+2        MOVE BIRCH NAME TO SCREEN          
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
VR12     OC    ARBI,ARBI                                                        
         BNZ   VR15                                                             
         OC    BIR,BIR                                                          
         BNZ   VR15                                                             
         MVI   ERROR,ONEMKT        MUST HAVE AT LEAST ONE                       
         LA    R2,STLMKTAH                                                      
         B     EDTERR                                                           
*                                                                               
VR15     GOTO1 ADDELEM                                                          
         XC    DBSELRMK,DBSELRMK                                                
         B     VR20                                                             
         EJECT                                                                  
VALNUM   NTR1                                                                   
         GOTO1 ANY                                                              
         MVI   ERROR,NOTNUM                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         BNE   EDTERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BNP   EDTERR                                                           
         ST    R1,FULL                                                          
         B     XIT                                                              
         SPACE 1                                                                
* READ FOR MARKET NUMBER ON DEMO FILE *                                         
         SPACE 1                                                                
VALMKT   NTR1                                                                   
         MVI   DBFUNCT,DBGETMK     VALIDATES MARKET                             
         MVC   DBAREC,AIO2         SET ADDRESS OF OUTPUT                        
         MVC   DBSELRMK,FULL+2                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,0           IS VALID MARKET                              
         BNE   VAL10               YES,THEN EXIT                                
         GOTO1 DEFINE,DMCB,=C'MNAME',DBLOCK,WORK                                
         B     XIT                                                              
VAL10    MVI   ERROR,INVMKT                                                     
         B     EDTERR                                                           
         DROP  R5                                                               
         EJECT                                                                  
* EDIT STATIONS *                                                               
         SPACE 1                                                                
VR20     MVI   ELCODE,SLSELQ                                                    
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         MVI   LSTFOR,X'FF'                                                     
         MVC   LSTFOR+1(L'LSTFOR-1),LSTFOR                                      
         MVI   NSTAS,0                                                          
         XR    RF,RF                                                            
         LA    R2,STLFOR1H         FIRST FORMAT ENTRY                           
         LA    R6,ELEM                                                          
         USING SLSELEM,R6                                                       
*                                                                               
VR30     XC    ELEM,ELEM                                                        
         LA    R7,SLSFORM                                                       
*                                                                               
VR40     XC    FORMAT,FORMAT                                                    
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    VR52                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BNH   VR52                                                             
*                                                                               
VR50     EX    R1,*+8                                                           
         B     VR52                                                             
         MVC   FORMAT(0),8(R2)                                                  
*                                                                               
VR52     CLC   FORMAT,LSTFOR                                                    
         BE    VR70                                                             
*                                                                               
         OC    ELEM,ELEM           TEST NEW ELEMENT                             
         BZ    VR60                                                             
         SR    RF,RF                                                            
         ICM   RF,1,NSTAS                                                       
         BZ    VR30                                                             
         MH    RF,=H'5'            LENGTH OF STATION LIST                       
         AH    RF,=H'6'            FIXED LENGTH PART OF ELEM                    
         STCM  RF,1,SLSLEN                                                      
         MVI   SLSELEM,SLSELQ                                                   
         GOTO1 ADDELEM                                                          
         MVI   NSTAS,0             CLEAR COUNTER                                
         B     VR30                                                             
*                                                                               
VR60     MVC   0(4,R7),FORMAT                                                   
         LA    R7,SLSTALST                                                      
*                                                                               
VR70     MVC   LSTFOR,FORMAT       SAVE CURRENT FORMAT TYPE                     
*                                                                               
         LA    R4,7                SET LOOP COUNTER                             
*                                                                               
VR80     BAS   RE,BUMP                                                          
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VR121A                                                           
*                                                                               
VR90     BAS   RE,VALSTA                                                        
*                                                                               
         OC    ARBI,ARBI           TEST IF ARB MARKET NUMBER GIVEN              
         BZ    VR100               NO                                           
         MVC   RMKT,ARBI                                                        
         MVI   DBSELSRC,C'A'                                                    
         CLI   CTRY,C'C'                                                        
         BNE   *+8                                                              
         MVI   DBSELSRC,C'M'                                                    
         BAS   RE,MKTSTA           VALIDATE STATION                             
         CLI   STERR,C'Y'                                                       
         BNE   VR100                                                            
         LA    R2,STLFOR1H         FIRST FORMAT FIELD                           
         MVI   ERROR,SUPPLIED                                                   
         MVC   WORK(4),ACTSTAT                                                  
         MVC   WORK+4(27),=C' IS NOT VALID IN ARB MARKET'                       
         B     EDTERR                                                           
*                                                                               
VR100    OC    BIR,BIR             TEST IF BIR MARKET NUMBER GIVEN              
         BZ    VR110               NO                                           
         MVC   RMKT,BIR                                                         
         MVI   DBSELSRC,C'N'                                                    
         BAS   RE,MKTSTA                                                        
         CLI   STERR,C'Y'                                                       
         BNE   VR110                                                            
         LA    R2,STLFOR1H         FIRST FIELD ON SCREEN                        
         MVI   ERROR,SUPPLIED      STATION NOT FOUND IN MKT                     
         MVC   WORK(4),ACTSTAT                                                  
         MVC   WORK+4(29),=C' IS NOT VALID IN BIRCH MARKET'                     
         B     EDTERR                                                           
*                                                                               
VR110    MVC   0(5,R7),ACTSTAT                                                  
         AI    NSTAS,X'1'          ELEMENT LENGTH COUNTER                       
         LA    R7,5(R7)                                                         
         BCT   R4,VR80                                                          
         B     VR122                                                            
*                                                                               
VR121    BAS   RE,BUMP                                                          
VR121A   DS    0H                                                               
         OR    R4,R4                                                            
         BZ    *+8                                                              
         BCT   R4,VR121            BUMP TO END OF LINE                          
VR122    BAS   RE,BUMP             BUMP TO NEXT FORMAT FIELD                    
         BNZ   VR40                                                             
*                                                                               
         OC    ELEM,ELEM           END OF TWA - TEST ELEMENT TO ADD             
         BZ    VR150                                                            
         SR    RF,RF                                                            
         ICM   RF,1,NSTAS                                                       
         BZ    VR150                                                            
         MH    RF,=H'5'            LENGTH OF STATION LIST                       
         AH    RF,=H'6'            FIXED LENGTH PART OF ELEM                    
         STCM  RF,1,SLSLEN                                                      
         MVI   SLSELEM,SLSELQ                                                   
         GOTO1 ADDELEM             ADD LAST ELEMENT                             
         B     VR150                                                            
*                                                                               
         SPACE 1                                                                
* MAKE SURE THERE IS AT LEAST ONE STATION ELEMENT                               
         SPACE 1                                                                
VR150    L     R6,AIO                                                           
         MVI   ELCODE,SLSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    VR170                                                            
         LA    R2,STLSTA1H         FIRST STATION LIST ENTRY                     
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
VR160    BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
*                                                                               
VR170    ZIC   R5,SLSLEN                                                        
         SR    R4,R4                                                            
         SH    R5,=H'6'                                                         
         D     R4,=F'5'                                                         
         LR    R0,R5               NUMBER ENTRIES IN R0                         
*                                                                               
         ZIC   RF,SLSLEN                                                        
         LA    R7,SLSTALST         BEGINNING ADDRESS                            
         LA    R5,SLSELEM                                                       
         LA    R4,5                INCREMENT                                    
         LA    R5,0(RF,R5)                                                      
         SH    R5,=H'6'            LIMIT ADDRESS                                
         GOTO1 XSORT,DMCB,(R7),(R0),5,5,0      SORT THE LIST                    
*                                                                               
VR180    CLC   0(5,R7),5(R7)       COMPARE PAIRWISE FOR DUPLICATES              
         BNE   VR200                                                            
         MVI   ERROR,SUPPLIED                                                   
         LA    R2,STLFOR1H         FIRST FIELD ON SCREEN                        
         MVC   WORK(4),0(R7)       BUILD ERRORMESSAGE                           
         MVC   WORK+4(29),=C' IS A DUPLICATE STATION ENTRY'                     
         B     EDTERR                                                           
*                                                                               
VR200    BXLE  R7,R4,VR180                                                      
*                                                                               
         B     VR160                                                            
         EJECT                                                                  
VALSTA   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         XC    ACTSTAT,ACTSTAT                                                  
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),C',=/-'                               
*                                                                               
         CLI   0(R4),3                                                          
         BL    EDTERR                                                           
         CLI   0(R4),4                                                          
         BH    EDTERR                                                           
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    EDTERR                                                           
         MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
         CLI   1(R4),2                                                          
         BH    EDTERR                                                           
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,STAAM                                                         
         BE    STA10                                                            
         EX    R5,STAFM                                                         
         BE    STA10                                                            
         EX    R5,STACO                                                         
         BE    STA10                                                            
         B     EDTERR                                                           
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACO    CLC   22(0,R4),=C'CO'                                                  
         SPACE 1                                                                
* VALIDATE STATION CALL LETTERS EXIST ON DEMO FILE *                            
         SPACE 1                                                                
STA10    DS    0H                                                               
         B     XIT                                                              
*                                                                               
MKTSTA   NTR1                      VAL STATIONS ON DEMO FILE                    
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBAREC,AIO2                                                      
         MVC   DBSELSTA,ACTSTAT    VALIDATE CALL LETTERS                        
         MVC   DBSELRMK,RMKT                                                    
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
*        CLI   DBERROR,0                                                        
*        BNE   MKT10                                                            
         CLC   DBACTRMK,RMKT                                                    
         BE    XIT                                                              
         MVI   STERR,C'Y'          STATION ERROR IS YES                         
         B     XIT                                                              
MKT10    MVI   ERROR,INVSTAT                                                    
         B     EDTERR                                                           
*                                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
*                                                                               
DR       XC    ELEM,ELEM                                                        
         MVC   ELEM(12),=X'0C00000000030000D9C1D900'                            
         CLI   CTRY,C'C'                                                        
         BNE   *+10                                                             
         MVC   ELEM+8(3),=C'BBR'                                                
         LA    R2,ELEM                                                          
         GOTO1 VVALSRC             FAKE THE SOURCE FIELD TO 'RA'                
         L     R6,AIO                                                           
         MVI   ELCODE,SLDELQ       DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SLDELEM,R6                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(L'SLDDESC),SLDDESC                                          
*                                                                               
         LA    R2,STLDESCH                                                      
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        MOVE DESCRIPTION                             
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         OC    SLDMKTA,SLDMKTA     TEST ARB MKY GIVEN                           
         BZ    DR5                                                              
         LA    R2,STLMKTAH                                                      
         SR    R0,R0                                                            
         ICM   R0,3,SLDMKTA                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LH    R0,SLDMKTA                                                       
         ST    R0,FULL                                                          
         MVI   DBSELSRC,C'A'                                                    
         CLI   CTRY,C'C'                                                        
         BNE   *+8                                                              
         MVI   DBSELSRC,C'M'                                                    
         BAS   RE,VALMKT           READ FOR MARKET NAME                         
         LA    R2,STLMKNAH                                                      
         MVC   8(30,R2),WORK+2     MOVE ARB NAME TO SCREEN                      
         OI    6(R2),X'80'                                                      
*                                                                               
DR5      OC    SLDMKTB,SLDMKTB     TEST BIR MKT GIVEN                           
         BZ    DR6                                                              
*                                                                               
         LA    R2,STLMKTBH                                                      
         SR    R0,R0                                                            
         ICM   R0,3,SLDMKTB                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LH    R0,SLDMKTB                                                       
         ST    R0,FULL                                                          
         BAS   RE,VALMKT           READ FOR MARKET NAME                         
         LA    R2,STLMKNBH                                                      
         MVC   8(30,R2),WORK+2     MOVE BIR NAME TO SCREEN                      
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
DR6      BAS   RE,BUMP             CLEAR TO END OF SCREEN                       
         BZ    DR7                                                              
*                                                                               
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    DR6                 BRANCH IF ONES                               
*                                                                               
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,TWACLC                                                        
         BNH   DR6                                                              
         EX    RE,TWAXC                                                         
         OI    6(R2),X'80'                                                      
         B     DR6                                                              
*                                                                               
DR7      LA    R2,STLFOR1H         FIRST FORMAT FIELD                           
*                                                                               
         L     R6,AIO                                                           
         USING SLSELEM,R6                                                       
         MVI   ELCODE,SLSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    DR12                                                             
         DC    H'0'                                                             
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
*                                                                               
DR12     XC    WORK(4),WORK                                                     
         MVC   WORK(4),SLSFORM     GET FORMAT CODE                              
*                                                                               
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        MOVE FORMAT                                  
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   RF,SLSLEN           GET ELEMENT LENGTH                           
         LA    R7,SLSTALST         BEGINNING STATION LIST                       
         LA    R4,5                INCREMENT                                    
         LA    R5,SLSELEM                                                       
         LA    R5,0(RF,R5)         END OF ELEMENT ADDRESS                       
         BCTR  R5,0                LIMIT ADDRESS                                
*                                                                               
DR13     LA    RF,7                SET MAX# ELEM/LINE                           
DR14     BAS   RE,BUMP                                                          
         XC    WORK(8),WORK                                                     
         MVC   WORK(4),0(R7)                                                    
*                                                                               
         LA    R1,WORK+3                                                        
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         MVC   2(1,R1),4(R7)                                                    
         MVI   3(R1),C'M'                                                       
*                                                                               
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        MOVE STATION                                 
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         BCTR  RF,0                ELEMENT/LINE COUNTER                         
         BXLE  R7,R4,*+8                                                        
         B     DR19                                                             
         OR    RF,RF                                                            
         BNZ   DR14                                                             
         BAS   RE,BUMP             BEGINNING NEXT LINE                          
         XC    WORK(4),WORK                                                     
         MVC   WORK(4),SLSFORM     GET FORMAT CODE                              
*                                                                               
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        MOVE FORMAT                                  
         OI    6(R2),X'80'         XMIT                                         
         B     DR13                                                             
*                                                                               
DR19     OR    RF,RF                                                            
         BZ    *+12                                                             
         BAS   RE,BUMP             BUMP TO NEXT END OF LINE                     
         BCT   RF,*-4                                                           
*                                                                               
         BAS   RE,BUMP                                                          
         B     DR10                                                             
*                                                                               
TWACLC   CLC   8(0,R2),SPACES                                                   
TWAXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING SLREC,R6                                                         
*                                                                               
         LA    R2,STLNAMH                                                       
         MVC   STLNAM,SLKNAME                                                   
         OI    6(R2),X'80'         SET XMT                                      
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     DS    0H                  NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),SVKEY        TEST SAME TYPE/AGY/MED                       
         BNE   LRX                                                              
*                                                                               
LR40     LA    R6,KEY                                                           
         USING SLREC,R6                                                         
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   LISTAR(8),SLKNAME                                                
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,SLDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SLDELEM,R6                                                       
         MVC   LISTAR+10(L'SLDDESC),SLDDESC                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* PRINT REPORT ROUTINE                                                          
*                                                                               
PR       LA    R1,HEDSPECS         SET UP HEADHOK AND SPECS                     
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HDHOOKOK,C'Y'                                                    
*                                                                               
         XC    KEY(13),KEY         POINT TO KEY                                 
         LA    R4,KEY                                                           
         USING SLKEY,R4                                                         
*                                                                               
         MVI   SLKTYPE,X'0D'                                                    
         MVI   SLKSUB,X'5B'                                                     
         MVC   SLKAM,BAGYMD                                                     
*                                                                               
         LA    R2,STLTYPH                                                       
         XC    SLKNAME,SLKNAME                                                  
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BE    *+10                                                             
         MVC   SLKNAME,8(R2)                                                    
         MVC   SVKEY(13),KEY                                                    
*                                                                               
PR10     GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(3),SVKEY                                                     
         BNE   PR110                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    PR40                                                             
         LLC   RF,5(R2)            PRINT RECORDS DESIRED                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   PR20                                                             
         CLC   8(0,R2),KEY+3                                                    
*                                                                               
*HAVE RECORD, PRINT ROUTINE FOLLOWS                                             
*                                                                               
PR40     MVI   RECFOUND,C'Y'                                                    
*                                                                               
         MVC   PRNAME,SLKNAME      ENTER NAME IN PRINT                          
*                                                                               
         L     R6,AIO                                                           
         USING SLDELEM,R6                                                       
         GOTO1 GETREC                                                           
         MVI   ELCODE,SLDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NEED DESCRIPTION ELEMENT                     
         MVC   PRDESC(30),SLDDESC      ENTER DESRIPTION IN PRINT                
*                                                                               
         CLC   SLDMKTA,=H'0'                                                    
         BE    PR45                                                             
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,3,SLDMKTA                                                     
         EDIT  (R5),(4,PRARB),ALIGN=LEFT                                        
*                                                                               
PR45     CLC   SLDMKTB,=C'0000'                                                 
         BE    PR50                                                             
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,3,SLDMKTB                                                     
         EDIT  (R5),(4,PRBIR),ALIGN=LEFT                                        
         DROP  R6                                                               
*                                                                               
PR50     L     R6,AIO                                                           
         USING SLSELEM,R6                                                       
         MVI   ELCODE,SLSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    PR60                                                             
         DC    H'0'                MUCT HAVE STATION FORMAT                     
*                                                                               
PR55     BAS   RE,NEXTEL                                                        
         BNE   PR70                                                             
*                                                                               
PR60     MVC   PRFORM(4),SLSFORM   MOVE FORMAT INTO PRINT                       
         ZIC   RF,SLSLEN                                                        
         LA    R7,SLSTALST                                                      
         LA    R4,5                INCREMENT                                    
         LA    R5,SLSELEM                                                       
         LA    R5,0(RF,R5)                                                      
         BCTR  R5,0                LIMIT ADDRESS                                
*                                                                               
PR65     MVC   PRSTATI(4),0(R7)                                                 
         LA    R1,PRSTATI+3         MASSAGE DATA                                
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'          INSERT HYPHEN                                
         MVC   2(1,R1),4(R7)                                                    
         MVI   3(R1),C'M'                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BXLE  R7,R4,PR65                                                       
         B     PR55                                                             
*                                                                               
PR70     MVI   ALLOWLIN,2          DRAW LINE BETWEEN RECORDS                    
         OC    ABOX,ABOX                                                        
         BZ    PR80                                                             
         L     R5,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R5                                                               
*                                                                               
PR80     B     PR20                                                             
*                                                                               
PR110    CLI   RECFOUND,C'Y'                                                    
         BE    PRX                                                              
         MVI   HDHOOKOK,C'N'                                                    
         MVC   PRNAME(16),=C'NO RECORDS FOUND'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     XIT                                                              
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
         CLI   HDHOOKOK,C'N'                                                    
         BE    HOOKX                                                            
         MVI   H3,0                                                             
         MVI   H4,0                                                             
         MVI   H5,0                                                             
*                                                                               
         LA    R5,H6                                                            
         USING PRLINE,R5                                                        
         MVC   PRNAME(4),=C'NAME'                                               
         MVC   PRDESC+10(11),=C'DESCRIPTION'                                    
         MVC   PRARB(7),=C'ARB MKT'                                             
         MVC   PRBIR(7),=C'BIR MKT'                                             
         MVC   PRFORM(6),=C'FORMAT'                                             
         MVC   PRSTATI(7),=C'STATION'                                           
*                                                                               
         OC    ABOX,ABOX                                                        
         BZ    HOOKX                                                            
*                                                                               
         L     R4,ABOX             ADDRESS OF BOX DESECT                        
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+13,C'L'                                                  
         MVI   BOXCOLS+26,C'C'                                                  
         MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+74,C'C'                                                  
         MVI   BOXCOLS+86,C'C'                                                  
         MVI   BOXCOLS+96,C'C'                                                  
         MVI   BOXCOLS+115,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HOOKX    B     XIT                                                              
*                                                                               
HEDSPECS SSPEC H1,2,C'MEDIA      SPOT RADIO'                                    
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,57,C'STATION LIST REPORT'                                     
         SSPEC H2,57,C'-------------------'                                     
         SSPEC H1,102,RUN                                                       
         SSPEC H2,102,REPORT                                                    
         SSPEC H2,120,PAGE                                                      
         DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
TRAPERR  GOTO1 VGETERR                                                          
*                                                                               
EDTERR   EQU   TRAPERR                                                          
*                                                                               
SCANERR  MVI   ERROR,INVALID                                                    
         GOTO1 VCURSERR                                                         
*                                                                               
BUMP     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BR    RE                                                               
         SPACE 1                                                                
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESE1D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD6D                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* WORK AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
SAVEDPL  DS    A                                                                
SEQNUM   DS    H                                                                
FIRSTIME DS    C                                                                
RECFOUND DS    C                                                                
HDHOOKOK DS    C                                                                
COUNTER  DS    CL1                                                              
FORMAT   DS    CL4                                                              
LSTFOR   DS    CL4                                                              
FOUND    DS    C                                                                
SAVRE    DS    A                                                                
ARBI     DS    CL2                                                              
BIR      DS    CL2                                                              
RMKT     DS    CL2                                                              
STERR    DS    C                                                                
NSTAS    DS    X                   STATION COUNTER                              
         DS    CL(L'OVWORK-(*-OVWORK))           SPARE                          
         EJECT                                                                  
SLRECD   DSECT                                                                  
       ++INCLUDE SPGENSTLST                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
*                                                                               
SPOOLD   DSECT                                                                  
*PRINT DSECT FOR REPORTS                                                        
         ORG   P                                                                
PRLINE   DS    CL17                                                             
PRNAME   DS    CL8                                                              
         DS    CL2                                                              
PRDESC   DS    CL36                                                             
         DS    CL2                                                              
PRARB    DS    CL10                                                             
         DS    CL2                                                              
PRBIR    DS    CL10                                                             
         DS    CL2                                                              
PRFORM   DS    CL6                                                              
         DS    CL8                                                              
PRSTATI  DS    CL10                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPRES21   04/01/14'                                      
         END                                                                    
