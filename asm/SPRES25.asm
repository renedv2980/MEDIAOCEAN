*          DATA SET SPRES25    AT LEVEL 049 AS OF 04/01/14                      
*PHASE T20F25A                                                                  
         TITLE 'T20F25- RESEARCH COMBO LIST MAINT/LIST'                         
T20F25   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F25**,RA,RR=R2                                              
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
         ST    R2,RELO                                                          
         LA    R2,GETEL                                                         
         ST    R2,AGETEL                                                        
         MVI   ACTELOPT,C'N'                                                    
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
         XC    KEY,KEY                                                          
         MVI   SUBSID,C'N'                                                      
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         BZ    VK5                                                              
         GOTO1 =A(CKSUBSID),DMCB,(R9),(RC),RR=RELO                              
         BNZ   VK5                                                              
         MVI   SUBSID,C'Y'                                                      
*                                                                               
VK5      LA    R5,KEY                                                           
         USING CLKEY,R5                                                         
         MVC   DATADISP,=H'24'                                                  
         XC    KEY,KEY                                                          
         MVI   CLKTYPE,X'0D'                                                    
         MVI   CLKSUB,X'5E'                                                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(12),=X'0C00000000030000D9C1D900'                            
         CLI   CTRY,C'C'                                                        
         BNE   *+10                                                             
         MVC   ELEM+8(3),=C'BBR'                                                
         LA    R2,ELEM                                                          
         GOTO1 VVALSRC             FAKE THE SOURCE FIELD TO 'RA'                
         MVC   CLKAM,BAGYMD                                                     
         CLI   SUBSID,C'Y'         IS CURRENT AGENCY SUBSID OF INTEREP?         
         BNE   AGY00001                                                         
         MVI   CLKAM,X'B2'         MAKE SURE WE USE INTEREP                     
*                                                                               
AGY00001 MVC   CLKMKAL,=CL3' '                                                  
         ZIC   R1,CBLMKTH+5                                                     
         LTR   R1,R1                                                            
         BZ    NOALPHA                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLKMKAL(0),CBLMKT   SAVE ALPHA CODE                              
NOALPHA  MVC   CLKRSRV,CBLSRCE     SAVE SOURCE                                  
         MVC   CLKBTYP,CBLBTYP     SAVE BOOK TYPE                               
*                                                                               
         LA    R2,CBLNAMH                                                       
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VKX                                                              
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK10     DS    0H                                                               
         GOTO1 ANY                                                              
         MVC   CLKNAME,WORK                                                     
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
         MVI   ELCODE,CLDELQ                                                    
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,CBLDESCH                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R6,ELEM             BUILD DESCRIPTION ELEMENT                    
         USING CLDELEM,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   CLDELEM,CLDELQ                                                   
         MVI   CLDLEN,CLDLENEQ                                                  
         MVC   CLDDESC,SPACES      PAD FIELD                                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CLDDESC(0),8(R2)                                                 
*                                                                               
         LA    R2,CBLMKTH                                                       
         CLI   5(R2),0             ARB MKT NOT MANDITORY                        
         BNE   VR5                                                              
         LA    R2,CBLMKTNH                                                      
         MVC   8(L'CBLMKTN,R2),SPACES     CLEAR MKT NAME                        
         OI    6(R2),X'80'                                                      
         B     VR15                                                             
*                                                                               
VR5      XC    BOOKTYPE,BOOKTYPE                                                
         MVI   MEDTYPE,C'R'                                                     
         MVC   SRCTYPE,CBLSRCE                                                  
         CLI   SRCTYPE,C'N'                                                     
         BNE   VR6                                                              
         MVI   SRCTYPE,C'B'        NIELSON IS BIRCH                             
VR6      MVC   CITYCODE,=CL3' '                                                 
         ZIC   R5,CBLMKTH+5                                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),CBLMKT                                               
CTYSRCH  LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         ST    R1,FULL                                                          
         MVC   CLKMKAL,CITYCODE                                                 
         MVC   ARBI,FULL+2         SAVE ARB CODE                                
         BAS   RE,VALMKT           READ FOR MARKET NAME                         
         LA    R2,CBLMKTNH                                                      
         MVC   8(L'CBLMKTN,R2),WORK+2        MOVE ARB NAME TO SCREEN            
         OI    6(R2),X'80'         XMIT                                         
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
         SPACE 3                                                                
MEDTYPE  DC    C'R'                                                             
SRCTYPE  DC    C'A'                                                             
BOOKTYPE DC    X'0'                                                             
CITYCODE DC    CL3'   '                                                         
         EJECT                                                                  
* EDIT STATIONS *                                                               
         SPACE 1                                                                
VR20     MVI   ELCODE,CLSELQ                                                    
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         MVI   LSTFOR,X'FF'                                                     
         MVC   LSTFOR+1(L'LSTFOR-1),LSTFOR                                      
         MVI   NSTAS,0                                                          
         XR    RF,RF                                                            
         LA    R2,CBLCMB1H         FIRST FORMAT ENTRY                           
         LA    R6,ELEM                                                          
         USING CLSELEM,R6                                                       
*                                                                               
VR30     XC    ELEM,ELEM                                                        
         LA    R7,CLSFORM                                                       
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
         AH    RF,=H'11'           FIXED LENGTH PART OF ELEM                    
         STCM  RF,1,CLSLEN                                                      
         MVI   CLSELEM,CLSELQ                                                   
         GOTO1 ADDELEM                                                          
         MVI   NSTAS,0             CLEAR COUNTER                                
         B     VR30                                                             
*                                                                               
VR60     MVC   0(9,R7),=CL9' '                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),FORMAT                                                   
         LA    R7,CLSTALST                                                      
*                                                                               
VR70     MVC   LSTFOR,FORMAT       SAVE CURRENT FORMAT TYPE                     
*                                                                               
         LA    R4,7                SET LOOP COUNTER                             
*                                                                               
VR80     BAS   RE,BUMP                                                          
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VR120                                                            
*                                                                               
VR90     BAS   RE,VALSTA                                                        
*                                                                               
         B     VR100               SKIP VALIDATING ALTOGETHER 3/3/92            
*                                                                               
         CLI   SRCTYPE,C'A'        TEST IF MARKET NUMBER GIVEN                  
         BE    VR91                NO                                           
         B     VR100                                                            
VR91     MVC   RMKT,ARBI                                                        
         MVI   DBSELSRC,C'A'                                                    
VR95     CLI   CTRY,C'C'                                                        
         BNE   *+8                                                              
         MVI   DBSELSRC,C'M'                                                    
         BAS   RE,MKTSTA                                                        
         CLI   STERR,C'Y'                                                       
         BNE   VR100                                                            
         CLC   DBSELSRC,SRCTYPE                                                 
         BE    VR98                                                             
         MVI   DBSELSRC,C'B'                                                    
         B     VR100                                                            
VR98     LA    R2,CBLCMB1H                                                      
         MVI   ERROR,SUPPLIED                                                   
         MVC   WORK(4),ACTSTAT                                                  
         MVC   WORK+4(27),=C' IS NOT VALID IN ARB MARKET'                       
         B     EDTERR                                                           
*                                                                               
VR100    MVC   RMKT,ARBI                                                        
         B     VR110               ADDED 3/3/92, NO MORE BIRCH                  
         MVI   DBSELSRC,C'N'                                                    
         BAS   RE,MKTSTA                                                        
         CLI   STERR,C'Y'                                                       
         BNE   VR110                                                            
         LA    R2,CBLCMB1H                                                      
         MVI   ERROR,SUPPLIED                                                   
         MVC   WORK(4),ACTSTAT                                                  
         MVC   WORK+4(29),=C' IS NOT VALID IN BIRCH MARKET'                     
         B     EDTERR                                                           
*                                                                               
VR110    MVC   0(5,R7),ACTSTAT                                                  
         AI    NSTAS,X'1'          ELEMENT LENGTH COUNTER                       
         LA    R7,5(R7)                                                         
         BCT   R4,VR80                                                          
*                                                                               
VR120    OR    R4,R4               SEE IF ANYTHING IN R4                        
         BZ    VR122                                                            
         BCTR  R4,0                                                             
VR121    BAS   RE,BUMP                                                          
         OR    R4,R4                                                            
         BZ    *+8                                                              
         BCT   R4,VR121            BUMP TO END OF LINE                          
         BAS   RE,BUMP             BUMP TO NEXT FORMAT FIELD                    
         BNZ   VR40                                                             
         B     VR123                                                            
*                                                                               
VR122    BAS   RE,BUMP             BUMP TO NEXT FORMAT FIELD                    
         BZ    VR123               END OF TWA                                   
         CLI   5(R2),0             CONTINUATION OF PREV LINE?                   
         BE    VR70                YES, KEEP ADDING STATIONS                    
         B     VR40                NEXT ELEMENT                                 
*                                                                               
VR123    OC    ELEM,ELEM           END OF TWA - TEST ELEMENT TO ADD             
         BZ    VR150                                                            
         SR    RF,RF                                                            
         ICM   RF,1,NSTAS                                                       
         BZ    VR150                                                            
         MH    RF,=H'5'            LENGTH OF STATION LIST                       
         AH    RF,=H'11'           FIXED LENGTH PART OF ELEM                    
         STCM  RF,1,CLSLEN                                                      
         MVI   CLSELEM,CLSELQ                                                   
         GOTO1 ADDELEM             ADD LAST ELEMENT                             
         B     VR150                                                            
*                                                                               
         SPACE 1                                                                
* MAKE SURE THERE IS AT LEAST ONE STATION ELEMENT                               
         SPACE 1                                                                
VR150    L     R6,AIO                                                           
         MVI   ELCODE,CLSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    VR170                                                            
         LA    R2,CBLST11H         FIRST STATION LIST ENTRY                     
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
VR160    BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
*                                                                               
VR170    ZIC   R5,CLSLEN                                                        
         SR    R4,R4                                                            
         SH    R5,=H'11'                                                        
         D     R4,=F'5'                                                         
         LR    R0,R5               NUMBER ENTRIES IN R0                         
*                                                                               
         ZIC   RF,CLSLEN                                                        
         LA    R7,CLSTALST         BEGINNING ADDRESS                            
         LA    R5,CLSELEM                                                       
         LA    R4,5                INCREMENT                                    
         LA    R5,0(RF,R5)                                                      
         SH    R5,=H'11'           LIMIT ADDRESS                                
         GOTO1 XSORT,DMCB,(R7),(R0),5,5,0      SORT THE LIST                    
*                                                                               
VR180    CLC   0(5,R7),5(R7)       COMPARE PAIRWISE FOR DUPLICATES              
         BNE   VR200                                                            
         MVI   ERROR,SUPPLIED                                                   
         LA    R2,CBLCMB1H         FIRST FIELD ON SCREEN                        
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
         BE    VALSTA1                                                          
         CLI   0(R4),5                                                          
         BH    EDTERR                                                           
         MVC   ACTSTAT,=CL5' '                                                  
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTSTAT(0),12(R4)                                                
         CLI   0(R4),5                                                          
         BNE   VALSTA2                                                          
         CLI   ACTSTAT+4,C'A'                                                   
         BE    STA10                                                            
         CLI   ACTSTAT+4,C'F'                                                   
         BE    STA10                                                            
         CLI   ACTSTAT+4,C'C'                                                   
         BE    STA10                                                            
         CLI   ACTSTAT+4,C'B'                                                   
         BE    STA10                                                            
         CLI   ACTSTAT+4,C'D'                                                   
         BE    STA10                                                            
         B     EDTERR                                                           
VALSTA1  TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    EDTERR                                                           
         MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
VALSTA2  CLI   1(R4),2                                                          
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
         EX    R5,STAFF                                                         
         BE    STA10                                                            
         EX    R5,STAAA                                                         
         BE    STA10                                                            
         B     EDTERR                                                           
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACO    CLC   22(0,R4),=C'CO'                                                  
STAFF    CLC   22(0,R4),=C'FF'                                                  
STAAA    CLC   22(0,R4),=C'AA'                                                  
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
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'****'                                                 
         MVI   ELCODE,CLDELQ       DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     DR2                                                              
         USING CLDELEM,R6                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(L'CLDDESC),CLDDESC                                          
*                                                                               
DR2      LA    R2,CBLDESCH                                                      
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
DR5      MVC   CITYCODE,=CL3' '                                                 
         ZIC   R1,CBLMKTH+5                                                     
         LTR   R1,R1                                                            
         BZ    DR5A                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),CBLMKT  GET THE SAVED ALPHA CODE                     
*        MVC   CBLMKT,CLKMKAL                                                   
DR5A     OI    CBLMKTH+6,X'80'     XMIT                                         
         LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         ST    R1,FULL                                                          
         BAS   RE,VALMKT           READ FOR MARKET NAME                         
         LA    R2,CBLMKTNH                                                      
         MVC   8(15,R2),WORK+2     MOVE ARB NAME TO SCREEN                      
         OI    6(R2),X'80'                                                      
*                                                                               
DRB4_6   LA    R2,CBLCMB1H                                                      
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
DR7      LA    R2,CBLCMB1H         FIRST FORMAT FIELD                           
*                                                                               
         L     R6,AIO                                                           
         USING CLSELEM,R6                                                       
         MVI   ELCODE,CLSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    DR12                                                             
         B     XIT                                                              
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
*                                                                               
DR12     XC    WORK(9),WORK                                                     
         MVC   WORK(9),CLSFORM     GET FORMAT CODE                              
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
         ZIC   RF,CLSLEN           GET ELEMENT LENGTH                           
         LA    R7,CLSTALST         BEGINNING STATION LIST                       
         LA    R4,5                INCREMENT                                    
         LA    R5,CLSELEM                                                       
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
         MVC   8(0,R2),WORK                                                     
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         BCTR  RF,0                ELEMENT/LINE COUNTER                         
         BXLE  R7,R4,*+8                                                        
         B     DR19                                                             
         OR    RF,RF                                                            
         BNZ   DR14                                                             
         BAS   RE,BUMP             BEGINNING NEXT LINE                          
         XC    WORK(9),WORK                                                     
         MVC   WORK(9),CLSFORM     GET FORMAT CODE                              
*                                                                               
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      MOVE FORMAT                                  
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
         USING CLREC,R6                                                         
*                                                                               
         LA    R2,CBLNAMH                                                       
         MVC   CBLNAM,CLKNAME                                                   
         OI    6(R2),X'80'         SET XMT                                      
         MVI   5(R2),X'05'                                                      
         MVC   CBLMKT,CLKMKAL                                                   
         LA    R5,3                                                             
         MVI   CBLMKTH+5,3                                                      
         OI    CBLMKTH+6,X'80'                                                  
         MVC   CBLSRCE(1),CLKRSRV                                               
         MVI   CBLSRCEH+5,1                                                     
         OI    CBLSRCEH+6,X'80'                                                 
         MVC   CBLBTYP,CLKBTYP                                                  
         MVI   CBLBTYPH+5,1                                                     
         OI    CBLBTYPH+6,X'80'                                                 
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       MVI   NLISTS,14                                                        
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
         CLI   CLPMKTH+5,0         --+                                          
         BE    LR15                  I                                          
         MVC   KEY+3(3),=CL3' '      I                                          
         ZIC   R1,CLPMKTH+5          I                                          
         BCTR  R1,0                  I                                          
         EX    R1,*+8                I                                          
         B     *+10                  I                                          
         MVC   KEY+3(0),CLPMKT       I                                          
         CLI   CLPSRCEH+5,0           \ KEY MUST BE FILLED WITH CORRECT         
         BE    LR15                   / MKT, SOURCE, TYPE AND NAME              
         MVC   KEY+6(1),CLPSRCE      I                                          
         CLI   CLPBTYPH+5,0          I                                          
         BE    LR15                  I                                          
         MVC   KEY+7(1),CLPBTYP      I                                          
         CLI   CLPNAMH+5,0           I                                          
         BE    LR15                  I                                          
         MVC   KEY+8(5),=CL5' '      I                                          
         ZIC   R1,CLPNAMH+5          I                                          
         BCTR  R1,0                  I                                          
         EX    R1,*+8                I                                          
         B     *+10                  I                                          
         MVC   KEY+8(0),CLPNAM     __I                                          
LR10     DS    0H                                                               
LR15     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     DS    0H                  NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),SVKEY        TEST SAME TYPE/AGY/MED                       
         BNE   LRX                                                              
*                                                                               
LR40     LA    R6,KEY                                                           
         USING CLREC,R6                                                         
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   LISTAR(5),CLKNAME                                                
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   LISTAR+10(4),=C'****'                                            
         MVI   ELCODE,CLDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     LR42                                                             
*                                                                               
         USING CLDELEM,R6                                                       
         MVC   LISTAR+10(L'CLDDESC),CLDDESC                                     
         DROP  R6                                                               
*                                                                               
LR42     GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         SPACE 2                                                                
LINECNT  DC    X'0'                                                             
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
         USING CLKEY,R4                                                         
*                                                                               
         MVI   CLKTYPE,X'0D'                                                    
         MVI   CLKSUB,X'5E'                                                     
         MVC   CLKAM,BAGYMD                                                     
         CLI   SUBSID,C'Y'                                                      
         BNE   AGY00003                                                         
         MVI   CLKAM,X'B2'         USE INTEREP                                  
*                                                                               
AGY00003 LA    R2,STLNAMH                                                       
         XC    CLKNAME,CLKNAME                                                  
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BE    PR05                                                             
         MVC   CLKNAME,=CL5' '                                                  
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   CLKNAME(0),8(R2)                                                 
         MVC   CLKMKAL,=CL3' '                                                  
         CLI   STLMKTH+5,0                                                      
         BE    PR05                                                             
         ZIC   R5,STLMKTH+5                                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   CLKMKAL(0),STLMKT                                                
         MVC   CLKRSRV,STLSRCE                                                  
         MVC   CLKBTYP,STLBTYP                                                  
PR05     MVC   SVKEY(13),KEY                                                    
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
         CLC   8(0,R2),KEY+8                                                    
*                                                                               
*HAVE RECORD, PRINT ROUTINE FOLLOWS                                             
*                                                                               
PR40     MVI   RECFOUND,C'Y'                                                    
*                                                                               
         MVC   PRNAME(5),CLKNAME   ENTER NAME IN PRINT                          
*                                                                               
         L     R6,AIO                                                           
         USING CLDELEM,R6                                                       
         GOTO1 GETREC                                                           
         MVI   ELCODE,CLDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    PR45                                                             
******   DC    H'0'                    NEED DESCRIPTION ELEMENT                 
         MVC   PRDESC(4),=C'****'      NO DESSCRIPTION FOUND                    
         B     PR50                                                             
PR45     MVC   PRDESC(30),CLDDESC      ENTER DESRIPTION IN PRINT                
*                                                                               
         MVC   PRARB(3),CLKMKAL                                                 
*                                                                               
PR50     L     R6,AIO                                                           
         USING CLSELEM,R6                                                       
         MVI   ELCODE,CLSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    PR60                                                             
         DC    H'0'                MUCT HAVE STATION FORMAT                     
*                                                                               
PR55     BAS   RE,NEXTEL                                                        
         BNE   PR70                                                             
*                                                                               
PR60     MVC   PRFORM(9),CLSFORM   MOVE FORMAT INTO PRINT                       
         ZIC   RF,CLSLEN                                                        
         LA    R7,CLSTALST                                                      
         LA    R4,5                INCREMENT                                    
         LA    R5,CLSELEM                                                       
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
         MVC   PRARB(7),=C'MARKET '                                             
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
         EJECT                                                                  
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
*--------------------------------------------------------------*                
* CKSUBSID: CHECK IF CURRENT AGENCY IS SUBSIDARY OF INTEREP    *                
*--------------------------------------------------------------*                
         DS    0H                                                               
CKSUBSID NMOD1 0,**CKSUBSID**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         BAS   RE,SWTCHREP                                                      
         BNZ   CKSBSDER                                                         
         LA    RE,KEY                                                           
         USING RREPKEY,RE                                                       
         MVI   RREPKTYP,1                                                       
         MVC   RREPKREP,=C'IR'     INTEREP                                      
         BAS   RE,REPHIGH                                                       
         BNZ   CKSBSDER                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CKSBSDER                                                         
         L     R6,AIO1                                                          
         BAS   RE,REPGETRC                                                      
         MVC   DATADISP,=H'34'                                                  
         MVI   ELCODE,2            JUST NEED SUBSIDS                            
         BAS   RE,GETEL                                                         
         BNE   CKSBSDER            THERE HAS TO BE A 2 ELEMENT                  
         USING RREPSUB,R6                                                       
         ZIC   R0,RREPSCNT         # OF SUBSIDS                                 
         LA    R6,RREPSCOD                                                      
CKSUBS   CLC   AGENCY,0(R6)        IS AGENCY IN SUBSID LIST?                    
         BE    CKSUBXST            YES, LEAVE                                   
         LA    R6,2(R6)                                                         
         BCT   R0,CKSUBS                                                        
CKSBSDER BAS   RE,SWTCHSPT                                                      
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         XMOD1                                                                  
CKSUBXST BAS   RE,SWTCHSPT                                                      
         SR    R1,R1                                                            
         XMOD1                                                                  
         EJECT                                                                  
*----------------------------------------------------------                     
* SWITCH INTO REP SYSTEM TO CHECK SUBSIDARIES OF INTEREP                        
*----------------------------------------------------------                     
SWTCHREP NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'08'          REP SYSTEM #                                 
         GOTO1 (RF),DMCB           JUMP INTO REP                                
*                                                                               
         CLI   4(R1),0             CHECK IF SYSTEM IS NOT OPEN                  
         BE    EQXIT2                                                           
         B     NEQXIT2             THERE WAS AN ERROR                           
         EJECT                                                                  
*----------------------------------------------------------                     
* SWITCH BACK TO SPOT SYSTEM                                                    
*----------------------------------------------------------                     
SWTCHSPT NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
*                                                                               
         CLI   4(R1),0             CHECK IF SYSTEM IS NOT OPEN                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EQXIT2              EVERYTHING OK                                
         EJECT                                                                  
*----------------------------------------------------------                     
* DATA MANAGER CALLS TO USE IN REP SYSTEM                                       
*----------------------------------------------------------                     
REPHIGH  MVC   COMMAND,=C'DMRDHI'                                               
         B     REPDIR                                                           
*                                                                               
REPDIR   NTR1                                                                   
         IC    R4,DMINBTS                                                       
         IC    R3,TERM                                                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR  ',KEYSAVE,KEY,   X        
               ((R3),0),0                                                       
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   NEQXIT2                                                          
         B     EQXIT2                                                           
*                                                                               
REPGETRC MVC   COMMAND,=C'GETREC'                                               
         NTR1                                                                   
         LA    R2,KEY+28           GET DISK ADDRESS                             
         IC    R3,TERM                                                          
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFIL  ',               X        
               (R2),AIO1,((R3),DMWORK),0                                        
         B     EQXIT2                                                           
*                                                                               
EQXIT2   CR    RB,RB               SET CC EQ                                    
         B     XIT2                                                             
*                                                                               
NEQXIT2  LTR   RB,RB               SET CC NOT EQ                                
*                                                                               
XIT2     XIT1                                                                   
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESE5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESC5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESDBD                                                       
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
SUBSID   DS    C                   SUBSIDIARY OF INTEREP?                       
RELO     DS    A                                                                
AGETEL   DS    A                   ADDRESS OF GETEL                             
         DS    CL(L'OVWORK-(*-OVWORK))           SPARE                          
         EJECT                                                                  
CLRECD   DSECT                                                                  
       ++INCLUDE SPGENCOMBO                                                     
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
PRFORM   DS    CL6                                                              
         DS    CL8                                                              
PRSTATI  DS    CL10                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SPRES25   04/01/14'                                      
         END                                                                    
