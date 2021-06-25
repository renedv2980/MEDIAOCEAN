*          DATA SET PPLFM0A    AT LEVEL 016 AS OF 12/17/13                      
*PHASE T4040AA,+0K                                                              
         TITLE 'T4040A  PRINT LOGICAL FILE MAINT. CLT-ADV SCREEN'               
*   CHANGE LOG                                                                  
*                                                                               
*  KWAN 12/02/2103  CHANGES FOR TWO CHARS SYSTEM LETTER                         
*                                                                               
*  SMYE 1/17/97     ADD NEW CONTROL - NO AOR CON ACCESS ($CON)                  
*                                                                               
*  BPLA 5/11/94     ADD NEW CONTROL - ISSUE DATE REQUIRED                       
*                                                                               
*  BPLA 11/22/93    SPECIAL ENTRY - "*D" TO DELETE THE AORADV ELEM              
*                                                                               
T4040A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T4040A                                                         
         L     RC,0(1)                                                          
         USING GENOLD,RC                                                        
         USING   T404FFD,RA                                                     
         EJECT                                                                  
         LA    R4,IOAREA                                                        
         LH    R5,=H'1000'                                                      
         BAS   RE,CLEARWRK                                                      
*                                                                               
         CLI   SCRNUM,X'FA'         SEE IF I HAVE CLT ADV SCREEN                
         BE    CLT2                                                             
         MVI   DSPSW,1           SET TO FORMAT                                  
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404FA'                                 
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNUM,X'FA'                                                     
*                                                                               
CLT2     DS    0H                                                               
         MVC   KEY+27(4),CLTADDR   READ CLTHDR                                  
         BAS   RE,GETREC                                                        
CLTSCRN  CLI   DSPSW,0                                                          
         BNE   FORMATC                                                          
*                      CLIENT ADV SCREEN IN TWA  SO EDIT IT UNLESS              
*                       ACTION=DISPLAY                                          
         CLI   BACT,X'09'                                                       
         BE    FORMATC                                                          
*****                                                                           
EDITA    DS    0H                      EDIT ADVERTISER ELEMENT                  
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'15'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDTA5                                                            
*                                        CHANGE - DELETE OLD ELEM               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6)                                    
*                                                                               
EDTA5    DS    0H                      BUILD NEW ADVERTISER ELEMENT             
         MVC   ELEM,SPACES                                                      
         MVC   ELEM(2),=X'1514'                                                 
         LA    R4,ELEM                                                          
         USING PCLTADVE,R4                                                      
         LA    R2,CLAAORH                                                       
         GOTO1 ANY                                                              
*                                                                               
         CLC   8(2,R2),=C'*D'      SPECIAL CODE TO DELETE ELEM                  
         BE    EDTA75                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLTAOR(0),CLAAOR                                                
         LA    R2,CLAADVH                                                       
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLTADV(0),CLAADV                                                
         XC    PCLTADVC,PCLTADVC   BINARY ZEROES IF NOT ENTERED                 
         LA    R2,CLAADVCH                                                      
         CLI   5(R2),0             CLIENT CODE NOT REQUIRED                     
         BE    EDTA10                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLTADVC(0),CLAADVC                                              
*                                    @@@@ DATES                                 
EDTA10   XC    WORK,WORK                                                        
         LA    R2,CLADATEH                                                      
         GOTO1 ANY                                                              
         LA    R3,FLDINV                                                        
         ZIC   R5,5(R2)                                                         
         L     RF,VTWA                                                          
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,((R5),8(R2)),(X'20',PVALOUT)                           
         CLI   DMCB+4,X'01'                                                     
         BE    ERROR                                                            
         CLI   DMCB+4,X'02'                                                     
         BE    ERROR                                                            
         LA    R5,PVALOUT                                                       
         USING PERVALD,R5                                                       
         MVC   PCLTASDT,PVALBSTA                                                
         MVC   PCLTAEDT,PVALBEND                                                
         DROP  R5                                                               
*                                                                               
*                                                                               
EDTA15   LA    R2,CLAAORSH         AOR SE NUMBER                                
*                                                                               
         MVC   PRTSYSC,CLAAORS                                                  
         OC    PRTSYSC,SPACES                                                   
         L     RF,VTWA                                                          
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,(0,DDNAME),PRTSYS,0                                    
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    EDTA30                                                           
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    EDTA30                                                           
         USING DDNAMED,RE                                                       
         MVC   PCLTAORS,DDNASENO   RETURN SE NUMBER                             
         J     EDTA40                                                           
         DROP  RE                                                               
*                                                                               
EDTA30   LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTA40   DS    0H                                                               
         XC    PCLTACON,PCLTACON   CLEAR CONTROL BYTES TO BINARY ZEROS          
         CLI   CLAPLR,C'Y'                                                      
         BNE   EDTA42                                                           
         OI    PCLTACON,X'01'                                                   
EDTA42   CLI   CLACRLC,C'Y'                                                     
         BNE   EDTA44                                                           
         OI    PCLTACON,X'02'                                                   
EDTA44   CLI   CLACLLC,C'Y'                                                     
         BNE   EDTA46                                                           
         OI    PCLTACON,X'04'                                                   
EDTA46   CLI   CLACRLB,C'Y'                                                     
         BNE   EDTA48                                                           
         OI    PCLTACON,X'08'                                                   
EDTA48   CLI   CLAASC,C'Y'                                                      
         BNE   EDTA50                                                           
         OI    PCLTACON,X'10'                                                   
EDTA50   CLI   CLANAC,C'Y'                                                      
         BNE   EDTA52                                                           
         OI    PCLTACON,X'20'                                                   
EDTA52   CLI   CLAACL,C'Y'                                                      
         BNE   EDTA54                                                           
         OI    PCLTACON,X'40'                                                   
*                                                                               
EDTA54   DS    0H                                                               
         CLI   CLAAID,C'Y'                                                      
         BNE   EDTA56                                                           
         OI    PCLTACON,X'80'                                                   
*                                                                               
EDTA56   DS    0H                                                               
         CLI   CLAACA,C'Y'                                                      
         BNE   EDTA58                                                           
         OI    PCLTACON+1,X'01'                                                 
*                                                                               
EDTA58   DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'FF'        FIND END OF REC                              
         BAS   RE,NEXTEL                                                        
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
*                                                                               
EDTA75   DS    0H                                                               
         BAS   RE,PUTREC                                                        
         B     FORMATC                                                          
*                                                                               
DONE     MVI   DONESW,1                                                         
         B     EXXMOD                                                           
************************************************************                    
FORMATC  DS    0H                                                               
*                                                                               
         FOUT  CLAAORH,SPACES,2                                                 
         FOUT  CLAADVH,SPACES,3                                                 
         FOUT  CLAADVCH,SPACES,3                                                
         FOUT  CLADATEH,SPACES,17                                               
         FOUT  CLAAORSH,SPACES,2                                                
         MVI   WORK,C'N'                                                        
         FOUT  CLAPLRH,WORK,1                                                   
         FOUT  CLACRLCH,WORK,1                                                  
         FOUT  CLACLLCH,WORK,1                                                  
         FOUT  CLACRLBH,WORK,1                                                  
         FOUT  CLAASCH,WORK,1                                                   
         FOUT  CLANACH,WORK,1                                                   
         FOUT  CLAACLH,WORK,1                                                   
         FOUT  CLAAIDH,WORK,1                                                   
         FOUT  CLAACAH,WORK,1                                                   
         FOUT  CLAASE2H,SPACES,2                                                
         FOUT  CLAPLR2H,SPACES,1                                                
         FOUT  CLACRL2H,SPACES,1                                                
         FOUT  CLACLL2H,SPACES,1                                                
         FOUT  CLACLB2H,SPACES,1                                                
         FOUT  CLAASC2H,SPACES,1                                                
         FOUT  CLANAC2H,SPACES,1                                                
         FOUT  CLAACL2H,SPACES,1                                                
         FOUT  CLAAID2H,SPACES,1                                                
         FOUT  CLAACA2H,SPACES,1                                                
*                                                                               
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'15'       LOOK FOR ADVERTISER ELEMENT                   
         BAS   RE,NEXTEL                                                        
         BNE   DONE                                                             
*                                                                               
         XC    WORK,WORK                                                        
         USING PCLTADVE,R6                                                      
         FOUT  CLAAORH,PCLTAOR,2                                                
         FOUT  CLAADVH,PCLTADV,3                                                
         FOUT  CLAADVCH,PCLTADVC,3                                              
         L     RF,VTWA                                                          
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,PCLTASDT),(5,WORK)                                  
         CLI   PCLTAEDT,X'FF'                                                   
         BE    FDAT10                                                           
         MVI   WORK+8,C'-'                                                      
         L     RF,VTWA                                                          
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,PCLTAEDT),(5,WORK+9)                                
FDAT10   FOUT  CLADATEH,WORK,17                                                 
*                                                                               
         LA    R3,PCLTAORS                                                      
         BAS   RE,SEFORM                                                        
         FOUT  CLAAORSH,WORK,2                                                  
*                                                                               
FCON00   DS    0H                                                               
         MVI   WORK,C'N'                                                        
         FOUT  CLAPLRH,WORK,1                                                   
         FOUT  CLACRLCH,WORK,1                                                  
         FOUT  CLACLLCH,WORK,1                                                  
         FOUT  CLACRLBH,WORK,1                                                  
         FOUT  CLAASCH,WORK,1                                                   
         FOUT  CLANACH,WORK,1                                                   
         FOUT  CLAACLH,WORK,1                                                   
         FOUT  CLAAIDH,WORK,1                                                   
         FOUT  CLAACAH,WORK,1                                                   
         MVI   WORK,C'Y'                                                        
         TM    PCLTACON,X'01'                                                   
         BNO   FCON10                                                           
         FOUT  CLAPLRH,WORK,1                                                   
FCON10   TM    PCLTACON,X'02'                                                   
         BNO   FCON20                                                           
         FOUT  CLACRLCH,WORK,1                                                  
FCON20   TM    PCLTACON,X'04'                                                   
         BNO   FCON30                                                           
         FOUT  CLACLLCH,WORK,1                                                  
FCON30   TM    PCLTACON,X'08'                                                   
         BNO   FCON40                                                           
         FOUT  CLACRLBH,WORK,1                                                  
FCON40   TM    PCLTACON,X'10'                                                   
         BNO   FCON50                                                           
         FOUT  CLAASCH,WORK,1                                                   
FCON50   TM    PCLTACON,X'20'                                                   
         BNO   FCON60                                                           
         FOUT  CLANACH,WORK,1                                                   
FCON60   TM    PCLTACON,X'40'                                                   
         BNO   FCON70                                                           
         FOUT  CLAACLH,WORK,1                                                   
FCON70   TM    PCLTACON,X'80'                                                   
         BNO   FCON80                                                           
         FOUT  CLAAIDH,WORK,1                                                   
FCON80   DS    0H                                                               
         TM    PCLTACON+1,X'01'                                                 
         BNO   FCON90                                                           
         FOUT  CLAACAH,WORK,1                                                   
*                                  SWITCH TO CONTROL SYSTEM                     
FCON90   DS    0H                                                               
         MVI   SVAORSE,0                                                        
*                                                                               
         L     RF,VTWA             TO DISPLAY CONTROL VALUES                    
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'          CONTROL SYSTEM GENDIR                        
         GOTO1 (RF),DMCB                                                        
         CLI   DMCB+4,0                                                         
         BNE   FSWT10                                                           
*                                                                               
         LA    R6,CTKEY                                                         
         XC    CTKEY,CTKEY                                                      
         USING ADVREC,R6                                                        
         MVI   ADVREC,ADVRECQ                                                   
         MVI   ADVTYP,ADVTYPQ                                                   
         MVI   ADVSYS,C'P'                                                      
         MVC   ADVMED,HDRMED                                                    
         MVC   ADVAOR,CLAAOR                                                    
         MVC   ADVADV,CLAADV                                                    
         MVC   ADVAGY,CLAAOR                                                    
         MVC   CTIO(32),CTKEY                                                   
         DROP  R6                                                               
         L     RF,VTWA                                                          
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=CL8'DMRDHI'),=CL8'GENDIR',CTKEY,CTKEY              
         CLC   CTKEY(32),CTIO                                                   
         BNE   FSWT10              NOT FOUND SWITCH BACK                        
*                                                                               
         L     RF,VTWA                                                          
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=CL8'GETREC'),=CL8'GENFIL',CTKEY+36,CTIO            
         LA    R6,CTIO                                                          
         LA    R6,42(R6)           BUMP TO FIRST ELEM                           
         MVI   ELCODE,X'10'       LOOK FOR BUYING AGENCY ELEMENT                
         CLI   0(R6),X'10'                                                      
         BE    FCON100                                                          
         BAS   RE,NEXTEL                                                        
         BNE   FSWT10                                                           
         USING AGYD,R6                                                          
FCON100  MVC   SVAORSE,AGYPRN                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,CTKEY                                                         
         XC    CTKEY,CTKEY                                                      
         USING ADVREC,R6                                                        
         MVI   ADVREC,ADVRECQ                                                   
         MVI   ADVTYP,ADVTYPQ                                                   
         MVI   ADVSYS,C'P'                                                      
         MVC   ADVMED,HDRMED                                                    
         MVC   ADVAOR,CLAAOR                                                    
         MVC   ADVADV,CLAADV                                                    
         MVC   ADVAGY,AGYALPHA                                                  
         MVC   CTIO(32),CTKEY                                                   
         DROP  R6                                                               
         L     RF,VTWA                                                          
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=CL8'DMRDHI'),=CL8'GENDIR',CTKEY,CTKEY              
         CLC   CTKEY(32),CTIO                                                   
         BNE   FSWT10              NOT FOUND SWITCH BACK                        
*                                                                               
         L     RF,VTWA                                                          
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=CL8'GETREC'),=CL8'GENFIL',CTKEY+36,CTIO            
         LA    R6,CTIO                                                          
         LA    R6,42(R6)           BUMP TO FIRST ELEM                           
         MVI   ELCODE,X'10'       LOOK FOR BUYING AGENCY ELEMENT                
         CLI   0(R6),X'10'                                                      
         BE    FSWT09                                                           
         BAS   RE,NEXTEL                                                        
         BNE   FSWT10                                                           
         USING AGYD,R6                                                          
FSWT09   DS    0H                                                               
         LA    R3,SVAORSE                                                       
         BAS   RE,SEFORM                                                        
         FOUT  CLAASE2H,WORK,2                                                  
*                                                                               
         MVC   CLAPLR2(1),AGYCNTL                                               
         OI    CLAPLR2H+5,X'80'                                                 
         MVC   CLACRL2(1),AGYCNTL+1                                             
         OI    CLACRL2H+5,X'80'                                                 
         MVC   CLACLL2(1),AGYCNTL+2                                             
         OI    CLACLL2H+5,X'80'                                                 
         MVC   CLACLB2(1),AGYCNTL+3                                             
         OI    CLACLB2H+5,X'80'                                                 
         MVC   CLAASC2(1),AGYCNTL+4                                             
         OI    CLAASC2H+5,X'80'                                                 
         MVC   CLANAC2(1),AGYCNTL+5                                             
         OI    CLANAC2H+5,X'80'                                                 
         MVC   CLAACL2(1),AGYCNTL+6                                             
         OI    CLAACL2H+5,X'80'                                                 
         MVC   CLAAID2(1),AGYCNTL+7                                             
         OI    CLAAID2H+5,X'80'                                                 
         MVC   CLAACA2(1),AGYCNTL+8                                             
         OI    CLAACA2H+5,X'80'                                                 
*                                                                               
FSWT10   L     RF,VTWA             SWITCH BACK TO PRINT                         
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                BETTER BE ABLE TO GET BACK                   
         DROP  R6                                                               
*****                                                                           
*                                                                               
         CLI   BACT,X'08'          SEE IF ACTION = CHANGE                       
         BNE   DONE                                                             
NOTDONE  DS    0H                                                               
         LA    R2,CLAAORH                                                       
         B     DONE                                                             
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
SEFORM   DS    0H                                                               
         LR    R0,RE                                                            
         MVC   WORK,SPACES                                                      
*                                                                               
         MVC   PRTSE#,0(R3)                                                     
         L     RF,VTWA                                                          
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,(0,DDNAME),PRTSE,0                                     
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    SEF20                                                            
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    SEF20                                                            
         USING DDNAMED,RE                                                       
         MVC   WORK(2),DDNASEID    RETURN SYSTEM LETTER                         
         DROP  RE                                                               
*                                                                               
SEF20    LR    RE,R0                                                            
         BR    RE                RETURN                                         
*                                                                               
         EJECT                                                                  
*                                                                               
ZEROS    DC    40C'0'                                                           
SPACES   DC    CL40' '                                                          
ELCODE   DS    CL1                                                              
ELEM     DS    CL30                                                             
DMWORK1  DS    12D                                                              
SAVEKEY  DS    CL32                                                             
SVAORSE  DS    CL1                 AOR SE NUMBER                                
*                                                                               
FLDINV   EQU   2                                                                
PROFERR  EQU   60                                                               
MISSMST  EQU   62                                                               
MCLTNF   EQU   63                                                               
PROFERR1 EQU   176                                                              
CACCERR  EQU   207                 LIMIT ACCESS ERROR                           
CMNTERR  EQU   53                                                               
VIRERR   DC    H'0'                                                             
*                                                                               
DDNAME   DC    CL8'DDNAME'                                                      
PRTSE    DC    C'SE=',X'0000'                                                   
         ORG   PRTSE+3                                                          
         DS    X                                                                
PRTSE#   DS    X                                                                
PRTOV#   DS    X                                                                
*                                                                               
PRTSYS   DC    C'S=P  '                                                         
         ORG   PRTSYS+3                                                         
PRTSYSC  DS    CL2                                                              
*                                                                               
OUTSE#   DS    X                                                                
OUTOV#   DS    X                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE PLFMWRK                                                        
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMFAD                                                       
         ORG   HDRLAST+2000                                                     
SENUM    DS    X                                                                
CTKEY    DS    CL50                                                             
CTIO     DS    CL1000                                                           
PVALOUT  DS    CL100        OUTPUT AREA FOR PERVAL                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENADVD                                                      
*                                                                               
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016PPLFM0A   12/17/13'                                      
         END                                                                    
