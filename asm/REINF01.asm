*          DATA SET REINF01    AT LEVEL 150 AS OF 02/21/03                      
*PHASE T80B01A,*                                                                
*INCLUDE REGENWLD                                                               
         TITLE 'T80B01 -- REINF01 -- INFO READER AND LISTER I'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*        REINF01 --- REP INFO PROGRAM READER/LISTER PART 1          *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG23/89 (MRR) --- HISTORY LOST                                   *           
*                     CHANGE DATAMGR CALL TO INCLUDE REPCODE        *           
*                                                                   *           
*  08/25/89  PJS  -- ADV/AGY RECS -- PASS IN REP CODE ON HI/SEQ     *           
*                    CALLS.  (FOR NEW DATAMGR)                      *           
*                                                                   *           
*  07/25/90  BU   -- STATION RECORD:  EI OPTION.  ACTIVE/INACTIVE   *           
*                    FILTER, DEFAULT BECOMES BOTH.                  *           
*                    AGENCY RECORD: EI OPTION.                      *           
*                    PRODUCT RECORD:  POINTER PERSON AND NET CON#   *           
*                    FILTERS, "NETWORK" OPTION FOR ALTERNATE DISPLAY*           
*                                                                   *           
*  06/08/92  SKU --- ADV REC: DISPLAY CAT CODE DESCRIPTION          *           
*                                                                   *           
*  11/11/92  SKU --- FAX DISPLAY OPTION FOR OFF AND SAL RECORDS     *           
*                                                                   *           
*  12/07/83  BU  --- NEW SALESPERSON FIELDS AND OPTIONS             *           
*                                                                   *           
*  FEB17/95  BU  --- DROP DISPLAY OF REGION HQ FIELDS               *           
*                                                                   *           
*  FEB23/95  BU  --- DON'T DISPLAY AGY/ADV 'CONTROL=' RECORDS       *           
*                                                                   *           
*  NOV09/95  RHV --- ROUNTINE TO SUPPORT KATZ OPTION FOR ADV        *           
*                    RECORDS - DISPLAYS KATZ CODES                  *           
*                                                                   *           
*  OCT03/96  RHV --- KEYWORD SEARCHING FOR ADV & SAL RECORDS        *           
*                                                                   *           
*  SEP02/98  AST --- ADDED NATIONAL/LOCAL FILTER FOR PAXSON         *           
*                    (R8) IS NOW A SECOND BASE REGISTER!!           *           
*                                                                   *           
*  SEP14/98  AST --- ADDED OFFICE FILTER FOR PAXSON                 *           
*                                                                   *           
*  APR22/99  RHV --- NAME SORT SALESMAN                             *           
*                                                                   *           
*  APR15/02  HQ  --- ADD EMAIL ADDRESS                              *           
*                                                                   *           
*  SEP25/02  HQ  --- DOULBE LINE DISPLAY TRASH TWA AREA BUG FIXED   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T80B01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**8B01**,R8,RR=R5                                              
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T80BFFD,RA                                                       
         ST    R5,RELO1            SAVE RELOCATION FACTOR                       
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,CLRKTAB          CLEAR KEYTABLE                               
         LA    R2,INFOUTH                                                       
         XC    INFOUT,INFOUT                                                    
         FOUT  (R2)                                                             
         SR    R7,R7                                                            
         L     R6,4(R1)            GET LINK                                     
         IC    R7,0(R6)                                                         
         B     BRANCH(R7)                                                       
         EJECT                                                                  
BRANCH   B     ADV10                                                            
         B     AGY10                                                            
         B     DIV10                                                            
         B     TEM10                                                            
         B     SALE0020                                                         
         B     REG10                                                            
*              OTHER RECORDS FOUND IN T80B02                                    
         DC    XL24'00'                                                         
         EJECT                                                                  
ADV10    LA    R3,KEYTAB                                                        
         CLC   SCPFLTR,ALL                                                      
         BE    ADV11               NO SCOPE FILTER, DISPLAY                     
         CLI   SCPFLTR,C'B'                                                     
         BE    ADV11               'BOTH' SCOPE FILTER, DISPLAY                 
         BAS   RE,LOADOFF                                                       
*                                                                               
ADV11    CLI   OPTNBYTE,15         'KATZ' DISPLAY OPTION?                       
         BE    KTZ50                                                            
         CLI   OPTNBYTE,2                                                       
         BE    ADV50                                                            
         LA    R4,ADV40                                                         
         CLI   NEXTBYTE,1                                                       
         BE    ADV12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'88'                                                        
         MVC   KEY+1(10),INFSTRT                                                
         OC    KEY+1(10),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
         MVI   COMPLEN,1           DEFAULT KEY COMPARISON LENGTH                
         TM    FLTRBYT3,X'04'      KEYWORD FILTER?                              
         BZ    ADV12               NO - SKIP KEYWORD ROUTINE                    
*                                                                               
*        THIS ROUTINE WILL RETURN THE APPROPRIATE VALUE FOR ADV NAME TO         
*        DO A READ HIGH FOR RECORDS MATCHING THE KEYWORD FILTER                 
*                                                                               
         GOTO1 =V(REGENWLD),DMCB,(10,KEYFLTR),(20,KEY+1),(X'80',0),RR=Y         
         ZIC   RE,4(R1)            LEN OF NAME STRING                           
         LA    RE,1(RE)            ADD 1 FOR REC TYPE                           
         STC   RE,COMPLEN          LENGTH TO COMPARE KEY FOR                    
*                                                                               
ADV12    DS    0H                                                               
         BAS   RE,HIGH                                                          
         ZIC   RE,COMPLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   SETDONE                                                          
         B     ADV30                                                            
*                                                                               
ADV20    EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         ZIC   RE,COMPLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   SETDONE                                                          
ADV30    CLI   REPBYTE,1           TEST STD RECS ONLY                           
         BE    ADV32                                                            
         CLC   REPALPHA,KEY+25                                                  
         BE    ADV35                                                            
*                                                                               
ADV32    CLI   REPBYTE,2           TEST REP REC ONLY                            
         BE    ADV20                                                            
         CLC   =C'ZZ',KEY+25                                                    
         BE    ADV35                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     ADV12                                                            
*                                                                               
ADV35    EQU   *                                                                
         CLC   =C'CONTROL',KEY+1   'CONTROL RECORD' KEY?                        
         BE    ADV20               YES - SKIP IT                                
         TM    FLTRBYT3,X'04'      KEYWORD FILTER?                              
         BZ    ADV38               NO - SKIP                                    
         GOTO1 =V(REGENWLD),DMCB,(10,KEYFLTR),(20,KEY+1),0,RR=Y                 
         TM    4(R1),X'80'         MATCHES WILDCARD FILTER?                     
         BZ    ADV20               NO                                           
ADV38    DS    0H                                                               
*** NEW CODE FOR NAT/LOCAL FILTER                                               
         CLC   SCPFLTR,ALL                                                      
         BE    ADV38A              NO SCOPE FILTER, CONTINUE                    
         CLI   SCPFLTR,C'B'                                                     
         BE    ADV39               'BOTH' SCOPE FILTER, DISPLAY                 
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   ADV20               NO, SKIP TO NEXT RECORD                      
         B     ADV39               DISPLAY RECORD                               
*                                                                               
*** NEW CODE FOR OFFICE FILTER                                                  
ADV38A   CLC   OFFLTR,ALL                                                       
         BE    ADV39               NO OFFICE FILTER, DISPLAY                    
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   ADV20               NO, SKIP TO NEXT RECORD                      
*                                                                               
ADV39    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE1,R2                                                         
         BR    R4                                                               
*                                                                               
ADV40    MVC   L1COD1,KEY+21                                                    
         MVC   0(L'L1COD1,R3),L1COD1         PUT CODE IN KEY TABLE              
         LA    R3,L'KEYTAB(R3)               BUMP TABLE POINTER                 
         MVC   L1NAM1,KEY+1                                                     
         LA    R4,ADV42                                                         
         B     ADV20                                                            
*                                                                               
ADV42    MVC   L1COD2,KEY+21                                                    
         MVC   0(L'L1COD2,R3),L1COD2         PUT CODE IN KEY TABLE              
         LA    R3,L'KEYTAB(R3)               BUMP TABLE POINTER                 
         MVC   L1NAM2,KEY+1                                                     
         LA    R4,ADV44                                                         
         B     ADV20                                                            
*                                                                               
ADV44    MVC   L1COD3,KEY+21                                                    
         MVC   0(L'L1COD3,R3),L1COD3         PUT CODE IN KEY TABLE              
         LA    R3,L'KEYTAB(R3)               BUMP TABLE POINTER                 
         MVC   L1NAM3,KEY+1                                                     
         LA    R4,ADV40                                                         
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R5,R2)                                                      
         BAS   RE,CLRLINE                                                       
         B     ADV20                                                            
         DROP  R2                                                               
         EJECT                                                                  
ADV50    CLI   NEXTBYTE,1                                                       
         BE    ADV52                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),INFSTRT                                                
         OC    KEY+21(4),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
ADV52    BAS   RE,HIGH                                                          
         CLI   KEY,X'08'                                                        
         BNE   SETDONE                                                          
         B     ADV70                                                            
         SPACE 1                                                                
ADV60    EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'08'                                                        
         BNE   SETDONE                                                          
*                                                                               
ADV70    CLI   REPBYTE,1           TEST FOR STD ONLY                            
         BE    ADV72                                                            
         CLC   REPALPHA,KEY+25                                                  
         BE    ADV75                                                            
*                                                                               
ADV72    CLI   REPBYTE,2           TEST REP RECS ONLY                           
         BE    ADV60                                                            
         CLC   =C'ZZ',KEY+25                                                    
         BE    ADV75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     ADV52                                                            
*                                                                               
ADV75    DS    0H                                                               
*** NEW CODE FOR NAT/LOCAL FILTER                                               
         CLC   SCPFLTR,ALL                                                      
         BE    ADV75A              NO SCOPE FILTER, CONTINUE                    
         CLI   SCPFLTR,C'B'                                                     
         BE    ADV76               'BOTH' SCOPE FILTER, DISPLAY                 
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   ADV60               NO, SKIP TO NEXT RECORD                      
         B     ADV76               DISPLAY REC                                  
*                                                                               
*** NEW CODE FOR OFFICE FILTER                                                  
ADV75A   CLC   OFFLTR,ALL                                                       
         BE    ADV76               NO OFFICE FILTER, DISPLAY                    
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   ADV60               NO, SKIP TO NEXT RECORD                      
*                                                                               
ADV76    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE3,R2                                                         
*                                                                               
ADV80    DS    0H                                                               
         CLC   SCPFLTR,ALL                                                      
         BE    ADV81               NO SCOPE FILTER, NO PREVIOUS GETREC          
         CLI   SCPFLTR,C'B'                                                     
         BE    ADV81               'BOTH' NO PREVIOUS GETREC                    
         LA    R6,RADVREC                                                       
         B     *+8                                                              
ADV81    BAS   RE,GETREC                                                        
*                                                                               
         CLC   =C'0000',RADVKADV   POSSIBLE 'CONTROL ID'?                       
         BNE   ADV82               NO  -                                        
         CLC   =C'CONTROL',RADVNAME                                             
*                                  YES - CONTROL RECORD?                        
         BE    ADV60               YES - SKIP IT                                
ADV82    EQU   *                                                                
         MVC   L3ADV,RADVKADV                                                   
         MVC   L3NAM,RADVNAME                                                   
         MVC   L3CITY,RADVCITY                                                  
         MVC   L3CLS,RADVCLSS                                                   
         MVC   L3CTG,RADVCATG                                                   
*                                                                               
         OC    RADVCATG,RADVCATG                                                
         BZ    ADV100                                                           
         XC    KEY,KEY             GET CATEGORY DESCRIPTION                     
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RADVCATG                                               
         BAS   RE,HIGH                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   ADV90                                                            
         CLC   RADVCATG,KEY+25                                                  
         BNE   ADV90                                                            
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         MVI   L3CTG+2,C'/'                                                     
         MVC   L3CTG+3(20),RCTGNAME                                             
ADV90    MVC   KEY,SAVEKEY         REESTABLISH SEQ ORDER                        
         BAS   RE,HIGH                                                          
*                                                                               
ADV100   FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     ADV60                                                            
         DROP  R2                                                               
         EJECT                                                                  
*** 12/87 - THIS CODE NO LONGER USED - AOF IS USED INSTEAD***                   
         SPACE 2                                                                
AGY10    CLI   OPTNBYTE,2                                                       
         BE    AGY50                                                            
         LA    R4,AGY40                                                         
         CLI   NEXTBYTE,1          NAME LIST FOR AGENCYS                        
         BE    AGY12                                                            
*                                  BUILD KEY                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'8A'           PASSIVE POINTER                              
         MVC   KEY+1(10),INFSTRT                                                
         OC    KEY+1(10),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
AGY12    BAS   RE,HIGH                                                          
         CLI   KEY,X'8A'                                                        
         BNE   SETDONE                                                          
         B     AGY30                                                            
         SPACE 1                                                                
AGY20    EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'8A'                                                        
         BNE   SETDONE                                                          
*                                                                               
AGY30    CLC   KEY+23(2),SPACES    GET ONLY TRUE AGENCY RECORDS                 
         BNE   AGY20                                                            
         CLI   REPBYTE,1                                                        
         BE    AGY32                                                            
         CLC   REPALPHA,KEY+25                                                  
         BE    AGY35                                                            
*                                                                               
AGY32    CLI   REPBYTE,2                                                        
         BE    AGY20                                                            
         CLC   =C'ZZ',KEY+25                                                    
         BE    AGY35                                                            
**       MVC   KEY+25(2),=C'ZZ'                                                 
**       B     AGY12                                                            
         B     AGY20               JUST SEQ                                     
*                                                                               
AGY35    EQU   *                                                                
         CLC   =C'CONTROL',KEY+1   'CONTROL RECORD' KEY?                        
         BE    AGY20               YES - SKIP IT                                
         MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE1,R2                                                         
         BR    R4                                                               
*                                                                               
AGY40    MVC   L1COD1,KEY+19                                                    
         MVC   L1NAM1(18),KEY+1                                                 
         LA    R4,AGY42                                                         
         B     AGY20                                                            
*                                                                               
AGY42    MVC   L1COD2,KEY+19                                                    
         MVC   L1NAM2(18),KEY+1                                                 
         LA    R4,AGY44                                                         
         B     AGY20                                                            
*                                                                               
AGY44    MVC   L1COD3,KEY+19                                                    
         MVC   L1NAM3(18),KEY+1                                                 
         LA    R4,AGY40                                                         
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R5,R2)                                                      
         BAS   RE,CLRLINE                                                       
         B     AGY20                                                            
         DROP  R2                                                               
         EJECT                                                                  
AGY50    CLI   NEXTBYTE,1          CODE LIST FOR AGENCYS                        
         BE    AGY52                                                            
*                                  BUILD KEY                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),INFSTRT                                                
         OC    KEY+19(6),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
AGY52    BAS   RE,HIGH                                                          
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
         B     AGY70                                                            
         SPACE 1                                                                
AGY60    EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
*                                                                               
AGY70    CLC   KEY+23(2),SPACES    GET ONLY TRUE AGENCY RECORDS                 
         BNE   AGY60                                                            
         CLI   REPBYTE,1                                                        
         BE    AGY72                                                            
         CLC   REPALPHA,KEY+25                                                  
         BE    AGY75                                                            
*                                                                               
AGY72    CLI   REPBYTE,2                                                        
         BE    AGY60                                                            
         CLC   =C'ZZ',KEY+25                                                    
         BE    AGY75                                                            
**       MVC   KEY+25(2),=C'ZZ'                                                 
**       B     AGY52                                                            
         B     AGY60                                                            
*                                                                               
*                                                                               
AGY75    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE5,R2                                                         
*                                                                               
AGY80    BAS   RE,GETREC                                                        
         CLC   =C'0000',RAGYKAGY   POSSIBLE 'CONTROL ID'?                       
         BNE   AGY82               NO  -                                        
         CLC   =C'CONTROL',RAGYNAM1                                             
*                                  YES - CONTROL RECORD?                        
         BE    AGY60               YES - SKIP IT                                
AGY82    EQU   *                                                                
         MVC   L5AGY,RAGYKAGY                                                   
         MVC   L5NAM,RAGYNAM1                                                   
         MVC   L5ADDR,RAGYADD1                                                  
         MVC   L5CITY(20),RAGYADD2                                              
*        LA    R8,L5CITY+19                                                     
*        BAS   RE,FLOAT                                                         
*        MVC   0(2,R8),RAGYSTAT                                                 
*        MVC   3(5,R8),RAGYZIP                                                  
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R5,R2)                                                      
         BAS   RE,CLRLINE                                                       
         B     AGY60                                                            
         DROP  R2                                                               
         EJECT                                                                  
SALE0020 LA    R3,KEYTAB                                                        
         CLI   NEXTBYTE,1                                                       
         BE    SALE0060                                                         
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         CLI   OPTNBYTE,1          SORT BY NAME?                                
         BNE   SALE0040                                                         
         MVI   KEY,X'86'                                                        
         MVC   KEY+2(2),REPALPHA                                                
         TM    FLTRBYT3,X'04'      KEYWORD FILTER?                              
         BO    *+10                YES - IGNORE START AT                        
         MVC   KEY+4(L'INFSTRT),INFSTRT                                         
         OC    KEY+4(20),SPACES                                                 
         B     SALE0060                                                         
*                                                                               
SALE0040 DS    0H                                                               
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),REPALPHA                                               
         TM    FLTRBYT3,X'04'      KEYWORD FILTER?                              
         BO    *+10                YES - IGNORE START AT                        
         MVC   KEY+24(3),INFSTRT                                                
         OC    KEY+24(3),SPACES                                                 
*                                                                               
SALE0060 BAS   RE,HIGH                                                          
         CLI   OPTNBYTE,1          SORT BY NAME?                                
         BNE   SALE0080                                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   NOREC                                                            
         B     SALE0140                                                         
SALE0080 DS    0H                                                               
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+22                                                  
         BNE   NOREC                                                            
         B     SALE0140                                                         
*                                                                               
SALE0100 BAS   RE,SEQ                                                           
         CLI   OPTNBYTE,1          SORT BY NAME?                                
         BNE   SALE0120                                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SETDONE                                                          
         B     SALE0140                                                         
SALE0120 DS    0H                                                               
         CLI   KEY,6                                                            
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+22                                                  
         BNE   SETDONE                                                          
*                                                                               
SALE0140 BAS   RE,GETREC                                                        
         TM    FLTRBYT3,X'04'      KEYWORD FILTER?                              
         BZ    SALE0160            NO - SKIP CHECK                              
         GOTO1 =V(REGENWLD),DMCB,(10,KEYFLTR),(20,RSALNAME),0,RR=Y              
         TM    4(R1),X'80'         MATCHES WILDCARD FILTER?                     
         BZ    SALE0100            NO                                           
SALE0160 DS    0H                                                               
         CLC   DIVFLTR,ALL         TEST DIVISION FILTER                         
         BE    SALE0180                                                         
         CLC   DIVFLTR,RSALTEAM                                                 
         BNE   SALE0100                                                         
SALE0180 CLC   TEMFLTR,ALL                                                      
         BE    SALE0200                                                         
         CLC   TEMFLTR,RSALTEAM+1                                               
         BNE   SALE0100                                                         
*                                                                               
SALE0200 CLC   OFFLTR,ALL                                                       
         BE    SALE0220                                                         
         CLC   OFFLTR,RSALOFF                                                   
         BNE   SALE0100                                                         
SALE0220 DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE6,R2                                                         
*                                                                               
         MVC   L6SAL,RSALKSAL                                                   
         MVC   0(L'L6SAL,R3),L6SAL       PUT CODE IN KEY TABLE                  
*                                                                               
         MVC   L6NAM,RSALNAME                                                   
         MVC   L6TEL,RSALTEL                                                    
         MVC   L6TEM,RSALTEAM                                                   
         MVC   L6OFF,RSALOFF                                                    
*                                                                               
         CLI   OPTNBYTE,25         EDI SCREEN DISPLAY?                          
         BE    SALE0280                                                         
         MVC   L6FAX,RSALFAX                                                    
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    SALE0260            NO  - ACTIVE                                 
         OC    ACFLTR,ACFLTR       ACCEPT ALL SALESPERSONS?                     
         BNZ   SALE0240            YES - DON'T CHECK LEAVE DATE                 
         GOTO1 VDATCON,DMCB,(5,WORK),(3,WORK)                                   
*                                  NO  - GET TODAY'S DATE                       
         CLC   RSALLEAV,WORK       LEAVE DATE VS TODAY'S DATE                   
         BNH   SALE0400            GONE - DON'T DISPLAY IT                      
SALE0240 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RSALLEAV),(5,L6LEAV)                             
*                                  INSERT DATE                                  
SALE0260 EQU   *                                                                
         CLI   RSALMGR,X'00'       ANY MANAGER?                                 
         BE    SALE0380            NO                                           
         CLI   RSALMGR,C'N'        AGAIN                                        
         BE    SALE0380            NO                                           
         MVC   L6MGR,=C'YES'       YES - FLAG IT                                
         B     SALE0420                                                         
SALE0280 EQU   *                                                                
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    SALE0300            NO  - ACTIVE                                 
         OC    ACFLTR,ACFLTR       ACCEPT ALL SALESPERSONS?                     
         BNZ   SALE0300            YES - DON'T CHECK LEAVE DATE                 
         GOTO1 VDATCON,DMCB,(5,WORK),(3,WORK)                                   
*                                  NO  - GET TODAY'S DATE                       
         CLC   RSALLEAV,WORK       LEAVE DATE VS TODAY'S DATE                   
         BNH   SALE0400            GONE - DON'T DISPLAY IT                      
SALE0300 EQU   *                                                                
         MVC   L6FAXPRF,=C'NO '    SET FAX PREF = NO                            
         TM    RSALFLG,X'02'       FAX PREFERENCE?                              
         BNO   SALE0320            OFF                                          
         MVC   L6FAXPRF,=C'YES'    SET FAX PREF = YES                           
SALE0320 EQU   *                                                                
         MVC   L6EDIUSE,=C'NO '    SET EDI USE  = NO                            
         TM    RSALFLG,X'20'       BLOCK EDI?                                   
         BO    SALE0340            YES                                          
         MVC   L6EDIUSE,=C'YES'    SET EDI USE  = YES                           
SALE0340 EQU   *                                                                
         CLC   RSALPOWR,=C'00'     UNINITIALIZED DATA?                          
         BE    SALE0360            YES - DON'T DISPLAY                          
         CLC   RSALPOWR,SPACES                                                  
         BNH   SALE0360            YES - DON'T DISPLAY                          
         MVC   L6PWRCDE,RSALPOWR   SET POWER CODE TO DISPLAY                    
         TM    RSALPFLG,X'80'      MINUS SET?                                   
         BNO   SALE0360            NO                                           
         MVI   L6MINUS,C'-'        YES - SET MINUS INDICATOR                    
SALE0360 EQU   *                                                                
SALE0380 DS    0H                                                               
         CLI   OPTNBYTE,12         MANAGERS ONLY LIST?                          
         BNE   SALE0420            NO  - DISPLAY IT                             
SALE0400 DS    0H                                                               
         BAS   RE,CLRLINE          YES - DON'T DISPLAY AND                      
*                                     CLEAR THE LINE                            
         XC    0(L'KEYTAB,R3),0(R3) CLEAR THE KEY TAB ENTRY                     
         B     SALE0100             READ ANOTHER RECORD                         
*                                                                               
SALE0420 DS    0H                  KEEP LINE AND KEY TAB ENTRY                  
         TM    OPT2BYTE,SASQ                                                    
         BZ    SALE0520                                                         
         MVC   SVKEY2,KEY                                                       
* SINGLE LINE SALES ASSISTANT INFO                                              
         XC    KEY,KEY                                                          
         MVI   KEY,X'46'                                                        
K        USING RSA2REC,KEY                                                      
         MVC   K.RSA2KREP,REPALPHA                                              
         MVC   K.RSA2KSAL,RSALKSAL                                              
         GOTO1 HIGH                                                             
         DROP  K                                                                
*                                                                               
         CLC   KEYSAVE(27),KEY                                                  
         BNE   SALE0480                                                         
         GOTO1 GETREC                                                           
         LA    R6,IOAREA                                                        
         USING RSA2REC,R6                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   SALE0480                                                         
*                                                                               
         USING RSASEMEM,R6                                                      
*        TM    OPT2BYTE,DLINEQ                                                  
*        BZ    SALE0460                                                         
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BNE   SALE0440                                                         
         MVC   KEY,SVKEY2                                                       
         B     SETNEXT                                                          
SALE0440 DS    0H                                                               
         BAS   RE,CLRLINE                                                       
         USING LINESAS,R2                                                       
SALE0460 DS    0H                                                               
         XC    LASASNM(LINESASX-LASASNM),LASASNM                                
         MVC   LASASNM,RSASEMNM                                                 
         CLI   RSASEMLN,L'RSASEMNM+2                                            
         BNH   SALE0500                                                         
*                                                                               
         MVC   LAEMFL,=CL3'YES'                                                 
         TM    RSASEMFL,X'80'                                                   
         BO    *+10                                                             
         MVC   LAEMFL,=CL3'NO'                                                  
*                                                                               
         CLI   RSASEMLN,L'RSASEMNM+L'RSASEMFL+2                                 
         BNH   SALE0500                                                         
         ZIC   RF,RSASEMLN                                                      
         SHI   RF,L'RSASEMNM+L'RSASEMFL+2                                       
         CHI   RF,L'LASASAML                                                    
         BL    *+8                                                              
         LHI   RF,L'LASASAML                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LASASAML(0),RSASEAML                                             
         DROP  R6                                                               
         B     SALE0500                                                         
SALE0480 MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                                                             
         TM    OPT2BYTE,SALONLYQ                                                
         BZ    SALE0520                                                         
         B     SALE0100                                                         
SALE0500 DS    0H                                                               
         MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                                                             
         B     SALE0520                                                         
*                                                                               
SALE0520 DS    0H                  KEEP LINE AND KEY TAB ENTRY                  
         LA    R3,L'KEYTAB(R3)     BUMP KEY TABLE POINTER                       
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     SALE0100                                                         
         DROP  R2                                                               
         EJECT                                                                  
REG10    LA    R3,KEYTAB                                                        
         CLI   NEXTBYTE,1                                                       
         BE    REG12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,3                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT                                                
         OC    KEY+25(2),SPACES                                                 
*                                                                               
REG12    BAS   RE,HIGH                                                          
         CLI   KEY,3                                                            
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     REG30                                                            
         SPACE 1                                                                
REG20    BAS   RE,SEQ                                                           
         CLI   KEY,3                                                            
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
*                                                                               
REG30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         BAS   RE,GETREC                                                        
         USING LINE12,R2                                                        
*                                                                               
REG40    MVC   L12REG1,RREGKREG                                                 
         MVC   L12NAM1,RREGNAME                                                 
*                                                                               
         MVC   0(L'L12REG1,R3),L12REG1        PUT ENTRY IN KEYTAB               
         LA    R3,L'KEYTAB(R3)                BUMP POINTER                      
*                                                                               
****>>>  MVC   L12TVHQ1,RREGTHQ                                                 
****>>>  MVC   L12RAHQ1,RREGRHQ                                                 
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     REG20                                                            
         DROP  R2                                                               
         EJECT                                                                  
DIV10    LA    R3,KEYTAB                                                        
         LA    R4,DIV40            SET OUTPUT AREA OF PRINT LINE                
         CLI   NEXTBYTE,1                                                       
         BE    DIV12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'05'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(1),INFSTRT                                                
         OI    KEY+25,C' '                                                      
*                                                                               
DIV12    BAS   RE,HIGH                                                          
         CLI   KEY,X'05'                                                        
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     DIV22                                                            
         SPACE 1                                                                
DIV20    BAS   RE,SEQ                                                           
         CLI   KEY,X'05'                                                        
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
DIV22    CLI   KEY+26,C' '                                                      
         BNE   DIV20                                                            
*                                                                               
DIV30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         BAS   RE,GETREC                                                        
         USING LINE1,R2                                                         
         BR    R4                                                               
*                                                                               
DIV40    MVC   L1COD1(2),RTEMKTEM                                               
         MVC   0(L'L1COD1,R3),L1COD1        INSERT IN KEYTAB                    
         LA    R3,L'KEYTAB(R3)              BUMP KEYTAB POINTER                 
         MVC   L1NAM1(10),RTEMDVNM                                              
         LA    R4,DIV42                                                         
         B     DIV20                                                            
*                                                                               
DIV42    MVC   L1COD2(2),RTEMKTEM                                               
         MVC   0(L'L1COD2,R3),L1COD2        INSERT IN KEYTAB                    
         LA    R3,L'KEYTAB(R3)              BUMP KEYTAB POINTER                 
         MVC   L1NAM2(10),RTEMDVNM                                              
         LA    R4,DIV44                                                         
         B     DIV20                                                            
*                                                                               
DIV44    MVC   L1COD3(2),RTEMKTEM                                               
         MVC   0(L'L1COD3,R3),L1COD3        INSERT IN KEYTAB                    
         LA    R3,L'KEYTAB(R3)              BUMP KEYTAB POINTER                 
         MVC   L1NAM3(10),RTEMDVNM                                              
         LA    R4,DIV40                                                         
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     DIV20                                                            
         DROP  R2                                                               
         EJECT                                                                  
TEM10    LA    R3,KEYTAB                                                        
         LA    R4,TEM40                                                         
         CLI   NEXTBYTE,1                                                       
         BE    TEM12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'05'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),INFSTRT                                                
         OC    KEY+25(2),SPACES                                                 
*                                                                               
TEM12    BAS   RE,HIGH                                                          
         CLI   KEY,X'05'                                                        
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+23                                                  
         BNE   NOREC                                                            
         B     TEM22                                                            
         SPACE 1                                                                
TEM20    BAS   RE,SEQ                                                           
         CLI   KEY,X'05'                                                        
         BNE   SETDONE                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   SETDONE                                                          
*                                                                               
TEM22    CLI   KEY+26,C' '                                                      
         BE    TEM20                                                            
         CLC   DIVFLTR,ALL                                                      
         BE    TEM30                                                            
         CLC   DIVFLTR,KEY+25                                                   
         BNE   TEM20                                                            
*                                                                               
TEM30    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         BAS   RE,GETREC                                                        
         USING LINE0,R2                                                         
         BR    R4                                                               
*                                                                               
TEM40    MVC   L0COD1,RTEMKTEM                                                  
         MVC   0(L'L0COD1,R3),L0COD1         PUT CODE IN KEY TABLE              
         LA    R3,L'KEYTAB(R3)               BUMP TABLE POINTER                 
         MVC   L0DIV1,RTEMDVNM                                                  
         MVC   L0TEM1,RTEMNAME                                                  
         LA    R4,TEM42                                                         
         B     TEM20                                                            
*                                                                               
TEM42    MVC   L0COD2,RTEMKTEM                                                  
         MVC   0(L'L0COD2,R3),L0COD2         PUT CODE IN KEY TABLE              
         LA    R3,L'KEYTAB(R3)               BUMP TABLE POINTER                 
         MVC   L0DIV2,RTEMDVNM                                                  
         MVC   L0TEM2,RTEMNAME                                                  
         LA    R4,TEM44                                                         
         B     TEM20                                                            
*                                                                               
TEM44    MVC   L0COD3,RTEMKTEM                                                  
         MVC   0(L'L0COD3,R3),L0COD3         PUT CODE IN KEY TABLE              
         LA    R3,L'KEYTAB(R3)               BUMP TABLE POINTER                 
         MVC   L0DIV3,RTEMDVNM                                                  
         MVC   L0TEM3,RTEMNAME                                                  
         LA    R4,TEM40                                                         
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     TEM20                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO DISPLAY KATZ CODES                                                 
*                                                                               
KTZ50    CLI   NEXTBYTE,1                                                       
         BE    KTZ52                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),INFSTRT                                                
         OC    KEY+21(4),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
KTZ52    BAS   RE,HIGH                                                          
         CLI   KEY,X'08'                                                        
         BNE   SETDONE                                                          
         B     KTZ70                                                            
         SPACE 1                                                                
KTZ60    EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'08'                                                        
         BNE   SETDONE                                                          
*                                                                               
KTZ70    CLI   REPBYTE,1           TEST FOR STD ONLY                            
         BE    KTZ72                                                            
         CLC   REPALPHA,KEY+25                                                  
         BE    KTZ75                                                            
*                                                                               
KTZ72    CLI   REPBYTE,2           TEST REP RECS ONLY                           
         BE    KTZ60                                                            
         CLC   =C'ZZ',KEY+25                                                    
         BE    KTZ75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     KTZ52                                                            
KTZ75    MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE3A,R2                                                        
*                                                                               
KTZ80    BAS   RE,GETREC                                                        
         CLC   =C'0000',RADVKADV   POSSIBLE 'CONTROL ID'?                       
         BNE   KTZ82               NO  -                                        
         CLC   =C'CONTROL',RADVNAME                                             
*                                  YES - CONTROL RECORD?                        
         BE    KTZ60               YES - SKIP IT                                
KTZ82    EQU   *                                                                
         MVC   L3AADV,RADVKADV                                                  
         MVC   L3AKATZ,RADVKATZ                                                 
         MVC   L3ANAM,RADVNAME                                                  
         MVC   L3ACITY,RADVCITY                                                 
         MVC   L3ACLS,RADVCLSS                                                  
         MVC   L3ACTG,RADVCATG                                                  
*                                                                               
         OC    RADVCATG,RADVCATG                                                
         BZ    KTZ100                                                           
         XC    KEY,KEY             GET CATEGORY DESCRIPTION                     
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RADVCATG                                               
         BAS   RE,HIGH                                                          
         CLC   REPALPHA,KEY+23                                                  
         BNE   KTZ90                                                            
         CLC   RADVCATG,KEY+25                                                  
         BNE   KTZ90                                                            
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         MVI   L3ASLASH,C'/'                                                    
         MVC   L3ACTGN,RCTGNAME                                                 
KTZ90    MVC   KEY,SAVEKEY         REESTABLISH SEQ ORDER                        
         BAS   RE,HIGH                                                          
*                                                                               
KTZ100   FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     KTZ60                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
CLRLINE  SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         SH    R5,=H'9'                                                         
         EX    R5,OCLINE                                                        
         BZR   RE                                                               
         EX    R5,XCLINE                                                        
         FOUT  (R2)                                                             
         BR    RE                                                               
*                                                                               
XCLINE   XC    8(0,R2),8(R2)                                                    
*                                                                               
OCLINE   OC    8(0,R2),8(R2)                                                    
         SPACE 3                                                                
NOREC    LA    R2,INFTITLH                                                      
NOREC2   BAS   RE,CLRLINE                                                       
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BNE   NOREC2                                                           
*                                                                               
         LA    R2,INFRCRDH                                                      
         BAS   RE,INVAL                                                         
         MVC   INFMESS(L'MSG3),MSG3                                             
         FOUT  INFMESSH                                                         
         B     EXIT                                                             
         SPACE 2                                                                
*LOAT    OI    0(R8),C' '                                                       
*        CLI   0(R8),C' '                                                       
*        BNE   FLOAT2                                                           
*        BCT   R8,FLOAT                                                         
*                                                                               
*LOAT2   LA    R8,2(R8)                                                         
*        BR    RE                                                               
         EJECT                                                                  
INVAL    NI    INFRCRDH+4,X'DF'                                                 
         NI    INFSTRTH+4,X'DF'                                                 
         NI    INFFLTRH+4,X'DF'                                                 
         NI    INFOPTNH+4,X'DF'                                                 
         MVI   NEXTBYTE,0                                                       
         BR    RE                                                               
         SPACE 2                                                                
SETNEXT  MVI   NEXTBYTE,1                                                       
         MVC   INFMESS(L'MSG2),MSG2                                             
         FOUT  INFMESSH                                                         
         LA    R2,INFNEXTH                                                      
         B     EXIT                                                             
         SPACE 2                                                                
SETDONE  TM    OPT2BYTE,SALONLYQ                                                
         BZ    *+14                                                             
         BAS   RE,CLRLINE                                                       
         XC    0(L'KEYTAB,R3),0(R3) CLEAR THE KEY TAB ENTRY                     
         B     X                                                                
         FOUT  (R2)                                                             
X        OC    INFOUT,INFOUT                                                    
         BZ    NOREC                                                            
         BAS   RE,INVAL                                                         
         MVC   INFMESS(L'MSG1),MSG1                                             
         FOUT  INFMESSH                                                         
*                                  CLEAR REST OF PAGE                           
DONE10   SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BE    DONE20                                                           
         BAS   RE,CLRLINE                                                       
         B     DONE10                                                           
*                                                                               
DONE20   EQU   *                                                                
         LA    R2,INFRCRDH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CHECK SCOPE FILTER FOR DISPLAYING ADV RECS                                    
*                                                                               
CHKSCP   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
*                                                                               
         BAS   RE,GETREC                                                        
         LA    R6,RADVREC                                                       
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   CSNX                NO ELEMS, IS NATIONAL                        
         B     CS11                                                             
CS10     BAS   RE,NEXTEL                                                        
         BNE   CSSX                NO MORE ELEMS, SKIP REC                      
*                                                                               
CS11     LA    R1,OFF2TBL          POINT TO TABLE OF OFFICES                    
         B     CSO02                                                            
CSO01    LA    R1,TBENTEQ(R1)      BUMP TO NEXT OFFICE IN TABLE                 
         CLI   0(R1),X'FF' END OF TABLE?                                        
         BNE   CSO02               NO, SKIP                                     
* END OF TABLE, OFFICE MUST BE NATIONAL                                         
         CLI   SCPFLTR,C'L'        IF LOCAL FILTER                              
         BNE   CSDX                NO, DISPLAY RECORD                           
         MVI   RTNFLG,1            LOCAL FILTER SET FLAG                        
         B     CS10                CHECK NEXT ELEM                              
CSO02    CLC   0(2,R1),2(R6)       COMPARE OFFICES                              
         BNE   CSO01               CHECK NEXT TABLE ENTRY                       
         CLC   SCPFLTR,2(R1)       COMPARE FILTER TO TABLE                      
         BE    CSDX                EQUAL, EXIT W/ FLAG SET TO DISPLAY           
*                                                                               
         B     CS10                CHECK NEXT ELEM                              
*                                                                               
CSNX     DS    0H                  REC HAS NO OFFICES, IS NATIONAL              
         CLI   SCPFLTR,C'L'        IS FILTER LOCAL?                             
         BNE   CSDX                NATIONAL/BOTH FILTER, DISPLAY                
*                                  LOCAL FILTER, NATIONAL RECORD, SKIP          
*                                                                               
CSSX     DS    0H                  SKIP THE RECORD                              
         MVI   RTNFLG,1            NO MATCHING SCOPE FOUND                      
*                                  DONT' DISPLAY                                
         B     CSX                                                              
*                                                                               
CSDX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
CSX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*CLEAR KEYTABLE IN REINFWRK                                                     
*                                                                               
CLRKTAB  NTR1                                                                   
         LHI   R3,NUMENTSK                                                      
         LA    R4,KEYTAB                                                        
CLR10    XC    0(L'KEYTAB,R4),0(R4)                                             
         LA    R4,L'KEYTAB(R4)                                                  
         BCT   R3,CLR10                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CHECK OFFICE FILTER FOR DISPLAYING ADV RECS                                   
*                                                                               
CHKOFF   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
*                                                                               
         BAS   RE,GETREC                                                        
         LA    R6,RADVREC                                                       
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   COSX                NO ELEMS, NO OFFICES, SKIP                   
         B     CO11                                                             
CO10     BAS   RE,NEXTEL                                                        
         BNE   COSX                NO MORE ELEMS, SKIP REC                      
*                                                                               
CO11     CLC   OFFLTR,2(R6)        IS OFFICE ELEMENT SAME AS FILTER?            
         BE    CODX                YES, DISPLAY RECORD                          
*                                                                               
         B     CO10                CHECK NEXT ELEM                              
*                                                                               
COSX     DS    0H                  SKIP THE RECORD                              
         MVI   RTNFLG,1            NO MATCHING SCOPE FOUND                      
*                                  DONT' DISPLAY                                
         B     COX                                                              
*                                                                               
CODX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
COX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOAD THE TABLE WITH OFFICES AND SCOPES                                        
*                                                                               
LOADOFF  NTR1                                                                   
         LA    RE,OFF2TBL          CLEAR OFFICE TABLE                           
         LA    RF,TBLNEQ                                                        
         XCEF                                                                   
*                                                                               
         LA    R4,OFF2TBL                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'44'           OFFICE2 RECORD                               
         MVC   KEY+23(2),REPALPHA                                               
*                                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
LO10     BAS   RE,SEQ                                                           
         CLC   KEY(25),KEYSAVE     SAME REP?                                    
         BNE   LOX                                                              
*                                                                               
         BAS   RE,GETREC                                                        
         LA    R6,ROFF2REC                                                      
         MVI   ELCODE,X'10'        GET OFFICE FAX ELEM                          
         BAS   RE,GETEL                                                         
         BNE   LO10                CHECK NEXT RECORD                            
         USING ROFF2FXE,R6                                                      
*                                                                               
         MVI   2(R4),C'N'          ASSUME NATIONAL                              
         TM    ROFF2PRF+1,X'80'    LOCAL?                                       
         BZ    *+8                 NO, SKIP                                     
         MVI   2(R4),C'L'          YES, IT'S LOCAL                              
         LA    R6,ROFF2REC                                                      
         USING ROFF2KEY,R6                                                      
         MVC   0(2,R4),ROFF2OFF    STORE OFFICE                                 
*                                                                               
         LA    R4,TBENTEQ(R4)      FILL IN NEXT TABLE ENTRY                     
         B     LO10                                                             
*                                                                               
LOX      DS    0H                                                               
         CLI   NEXTBYTE,1          SECOND TIME THRU?                            
         BNE   LOXX                FIRST TIME, EXIT                             
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
LOXX     XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
MSG1     DC    C'ACTION COMPLETED - ENTER NEXT REQUEST'                         
MSG2     DC    C'MORE RECORDS AVAILABLE - HIT ''ENTER'' TO CONTINUE'            
MSG3     DC    C'NO RECORDS ON FILE - ENTER NEXT REQUEST'                       
SPACES   DC    CL10' '                                                          
ALL      DC    5X'00'                                                           
ELCODE   DC    1X'00'                                                           
*                                                                               
RTNFLG   DS    CL1                                                              
SVKEY2   DS    CL32                                                             
*                                                                               
OFF2TBL  DS    0CL300              100 3-BYTE ENTRIES                           
         DS    CL2                 OFF CODE                                     
         DS    CL1                 OFF SCOPE                                    
*                                                                               
TBENTEQ  EQU   *-OFF2TBL LENGTH OF ONE ENTRY                                    
*                                                                               
         DS    99CL3               REST OF ENTRIES                              
*                                                                               
TBLNEQ   EQU   *-OFF2TBL LENGTH OF TABLE                                        
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
       ++INCLUDE REINFWRK                                                       
GENSAL2  DSECT                                                                  
       ++INCLUDE REGENSAL2                                                      
LINESAS  DSECT                                                                  
         DS    CL8                                                              
LASAL    DS    CL3                                                              
         DS    CL2                                                              
LANAME   DS    CL20                                                             
         DS    CL1                                                              
LASASNM  DS    CL20                                                             
         DS    CL1                                                              
LAEMFL   DS    CL3                                                              
         DS    CL1                                                              
LASASAML DS    CL28                                                             
LINESASX EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'150REINF01   02/21/03'                                      
         END                                                                    
