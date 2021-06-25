*          DATA SET REINF03S   AT LEVEL 255 AS OF 05/01/02                      
*PHASE T80B03A,*                                                                
*INCLUDE REGENWLD                                                               
         TITLE 'T80B03 - REINF03 - INFO READER AND LISTER III'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*        REINF03 --- REP INFO READER/LISTER PART 3                  *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 10OCT96 RHV  WELL, REINF02 GOT REAL BIG AND HAD NO MORE ROOM SO   *           
*              IT WAS TIME TO SPAWN THIS                            *           
*                                                                   *           
* 03SEP98 AST  ADDED NATIONAL/LOCAL OFFICE SCOPE FILTER FOR AOF     *           
*                                                                   *           
* 14SEP98 AST  ADDED OFFICE FILTER FOR AOF                          *           
*                                                                   *           
* 12MAY99 BU   TERRITORY INFORMATION DISPLAY FOR AGENCY             *           
*                                                                   *           
* 22JUN99 AST  TER+ OPTION FIXED                                    *           
*                                                                   *           
*                ***  END TOMBSTONE  ***                            *           
*********************************************************************           
*                                                                               
T80B03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**8B03**,R9,RR=R5                                              
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T80BFFD,RA                                                       
*                                                                               
         ST    R5,RELO1            SAVE RELO FACTOR                             
         MVC   KEY,SAVEKEY                                                      
         LA    R2,INFOUTH                                                       
         XC    INFOUT,INFOUT                                                    
         FOUT  (R2)                                                             
         SR    R7,R7                                                            
         L     R6,4(R1)            GET LINK                                     
         IC    R7,0(R6)                                                         
         B     BRANCH(R7)                                                       
         SPACE 2                                                                
*                                                                               
* NOTE: THE FOLLOWING TABLE PROVIDES BRANCH ADDRESSES FOR THE VARIOUS           
* RECORD TYPE HANDLING ROUTINES. IT'S A BIT MESSY BECAUSE THE BRANCH            
* INTRUCTIONS ARE POSITION SENSITIVE IN THE TABLE. IT WOULD BE A GOOD           
* IDEA TO LOOK AT THE 01 OR 02 MODULES FOR A BETTER IDEA OF HOW THIS            
* TABLE LOOKS WITH MORE RECORDS IN IT BEFORE YOU CHANGE IT.                     
*                                                                               
BRANCH   DC    XL24'00'                                                         
*        THESE RECORDS ARE REFERENCED IN T80B01                                 
         DC    6H'0'                                                            
*        THESE RECORDS ARE REFERENCED IN T80B02                                 
         B     AOF10               THIS IS HANDLED HERE!                        
         DC    26H'0'                                                           
*        THESE RECORDS ARE REFERENCED IN T80B02                                 
         EJECT                                                                  
AOF10    DS    0H                                                               
         BAS   RE,CLRKTAB          CLEAR KEY TABLE                              
         LA    R7,KEYTAB                                                        
         CLC   SCPFLTR,ALL                                                      
         BE    AOF10A                                                           
         CLI   SCPFLTR,C'B'                                                     
         BE    AOF10A                                                           
         BAS   RE,LOADOFF                                                       
AOF10A   LA    RE,INFOUTH                                                       
AOF11    DS    0H                  CLEAR THE SCREEN                             
         OI    6(RE),X'80'                                                      
         MVC   8(79,RE),SPACES                                                  
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   AOF11                                                            
*                                                                               
         CLI   OPTNBYTE,2                                                       
         BE    AOF50                                                            
         CLI   OPTNBYTE,10         RISK OPTION                                  
         BE    AOF100                                                           
         CLI   OPTNBYTE,11         LIAB OPTION                                  
         BE    AOF150                                                           
         CLI   OPTNBYTE,4          FAX OPTION                                   
         BE    AOF200                                                           
         CLI   OPTNBYTE,21         TERRITORY OPTION                             
         BE    AOF300                                                           
         CLI   OPTNBYTE,22         AGY W/ TERR                                  
         BE    AOF400                                                           
         LA    R4,AOF40                                                         
         CLI   NEXTBYTE,1                                                       
         BE    AOF12                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'8A'                                                        
         MVC   KEY+1(10),INFSTRT                                                
         OC    KEY+1(10),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
         MVI   COMPLEN,1           DEFAULT KEY COMPARISON LENGTH                
         TM    FLTRBYT3,X'04'      KEYWORD FILTER?                              
         BZ    AOF12               NO - SKIP KEYWORD ROUTINE                    
*                                                                               
*        THIS ROUTINE WILL RETURN THE APPROPRIATE VALUE FOR AGY NAME TO         
*        DO A READ HIGH FOR RECORDS MATCHING THE KEYWORD FILTER                 
*                                                                               
         GOTO1 =V(REGENWLD),DMCB,(10,KEYFLTR),(18,KEY+1),(X'80',0),RR=Y         
         ZIC   RE,4(R1)            LEN OF NAME STRING                           
         LA    RE,1(RE)            ADD 1 FOR REC TYPE                           
         STC   RE,COMPLEN          LENGTH TO COMPARE KEY FOR                    
*                                                                               
AOF12    DS    0H                                                               
         BAS   RE,HIGH                                                          
         ZIC   RE,COMPLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   SETDONE                                                          
         B     AOF30                                                            
*                                                                               
AOF20    DS    0H                                                               
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         ZIC   RE,COMPLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   SETDONE                                                          
*                                                                               
AOF30    CLC   AGYFLTR,ALL                                                      
         BE    AOF32                                                            
         CLC   AGYFLTR,KEY+19                                                   
         BNE   AOF20                                                            
*                                                                               
AOF32    CLI   REPBYTE,1                                                        
         BE    AOF34                                                            
         CLC   REPALPHA,KEY+25                                                  
         BE    AOF35                                                            
*                                                                               
AOF34    CLI   REPBYTE,2                                                        
         BE    AOF20                                                            
         CLC   =C'ZZ',KEY+25                                                    
         BE    AOF35                                                            
         B     AOF20               SEQ TO NEXT                                  
*                                                                               
AOF35    DS    0H                                                               
         TM    FLTRBYT3,X'04'      KEYWORD FILTER?                              
         BZ    AOF36               NO - SKIP                                    
         GOTO1 =V(REGENWLD),DMCB,(10,KEYFLTR),(18,KEY+1),0,RR=Y                 
         TM    4(R1),X'80'         MATCHES WILDCARD FILTER?                     
         BZ    AOF20               NO                                           
AOF36    DS    0H                                                               
         BAS   RE,GETREC           ES - NEEDS RECORD                            
         CLI   OPT2BYTE,7          EI AGENCIES ONLY?                            
         BNE   AOF37               NO - ACCEPT ALL                              
         CLI   RAGYPRO1,C'Y'       EASI AGENCY?                                 
         BNE   AOF20               NO - SKIP IT                                 
*                                                                               
AOF37    EQU   *                                                                
         CLC   =C'CONTROL',KEY+1   'CONTROL RECORD'?                            
         BE    AOF20               YES - SKIP IT                                
******                                                                          
* CODE FOR SCOPE FILTER                                                         
         CLC   SCPFLTR,ALL         NO SCOPE FILTER?                             
         BE    AOF37A              CONTIUE                                      
         CLI   SCPFLTR,C'B'        'BOTH' FILTER?                               
         BE    AOF38               PRINT ALL                                    
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF20               NO, SKIP TO NEXT REC                         
         B     AOF38                                                            
*                                                                               
*** NEW CODE FOR OFFICE FILTER                                                  
AOF37A   CLC   OFFLTR,ALL                                                       
         BE    AOF38               NO OFFICE FILTER, DISPLAY                    
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF20               NO, SKIP TO NEXT RECORD                      
*                                                                               
AOF38    EQU   *                                                                
         MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
***      CLI   OPTNBYTE,22         AGY W/TERR?                                  
***      BE    AOF39               YES                                          
         CLI   OPTNBYTE,23         AGY W/O TERR?                                
         BE    AOF39               YES                                          
         CLI   OPTNBYTE,24         ALL AGY + TERR?                              
         BNE   AOF39Z              YES                                          
AOF39    EQU   *                                                                
*                                                                               
*   KEY CONTAINS X'8A' NAME PASSIVE AT THIS POINT                               
*                                                                               
         MVI   KEY,X'1A'           SET TO ACCESS 2NDARY RECORD                  
         XC    KEY+1(18),KEY+1     CLEAR NAME FROM RECORD                       
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
         BE    *+6                 YES                                          
         DC    H'0'                NO  - KEY MUST BE ON FILE                    
         BAS   RE,GETREC           GET AGENCY2 RECORD                           
         MVC   KEY,SAVEKEY         RESET KEY                                    
         BAS   RE,HIGH             RESTART ORIGINAL KEY SEQUENCE                
         MVC   TEMPTERR,RAGY2TER                                                
         OC    TEMPTERR,SPACES     SET TO SPACE IF BINARY ZERO                  
         CLI   OPTNBYTE,22         AGY W/TERR?                                  
         BNE   AOF39A              NO                                           
         CLC   TEMPTERR,SPACES     ANY TERRITORY?                               
         BE    AOF20               NO  - SKIP RECORD                            
         B     AOF39Z                                                           
AOF39A   EQU   *                                                                
         CLI   OPTNBYTE,23         AGY W/O TERR?                                
         BNE   AOF39B              NO                                           
         CLC   TEMPTERR,SPACES     ANY TERRITORY?                               
         BNE   AOF20               YES - SKIP RECORD                            
         B     AOF39Z                                                           
AOF39B   EQU   *                                                                
         CLI   OPTNBYTE,24         ALL AGY + TERR?                              
         BE    AOF39Z              YES                                          
         DC    H'0'                NO  - UNRECOGNIZED                           
         USING LINE2,R2                                                         
AOF39Z   EQU   *                                                                
         BR    R4                                                               
TEMPTERR DC    CL2'  '             TEMPORARY TERRITORY STORAGE                  
*                                                                               
AOF40    MVC   L2AGY1,KEY+19                                                    
         MVC   L2AOF1,KEY+23                                                    
         MVC   L2NAM1,KEY+1                                                     
         MVC   L2NAM1+26(2),TEMPTERR                                            
         BAS   RE,BLDAGYK             PUT KEY IN KEYTAB                         
         LA    R7,L'KEYTAB(R7)        BUMP KEYTAB POINTER                       
         LA    R4,AOF42                                                         
         B     AOF20                                                            
*                                                                               
AOF42    MVC   L2AGY2,KEY+19                                                    
         MVC   L2AOF2,KEY+23                                                    
         MVC   L2NAM2,KEY+1                                                     
         MVC   L2NAM2+26(2),TEMPTERR                                            
         BAS   RE,BLDAGYK             PUT KEY IN KEYTAB                         
         LA    R7,L'KEYTAB(R7)        BUMP KEYTAB POINTER                       
         LA    R4,AOF40                                                         
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     AOF20                                                            
         DROP  R2                                                               
         EJECT                                                                  
AOF50    DS    0H                                                               
         CLI   NEXTBYTE,1                                                       
         BE    AOF52                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),INFSTRT                                                
         OC    KEY+19(6),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
AOF52    BAS   RE,HIGH                                                          
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
         B     AOF70                                                            
         SPACE 1                                                                
AOF60    EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
*                                                                               
AOF70    CLC   AGYFLTR,ALL                                                      
         BE    AOF72                                                            
         CLC   AGYFLTR,KEY+19                                                   
         BNE   AOF60                                                            
*                                                                               
AOF72    CLI   REPBYTE,1                                                        
         BE    AOF74                                                            
         CLC   REPALPHA,KEY+25                                                  
         BE    AOF75                                                            
*                                                                               
AOF74    CLI   REPBYTE,2                                                        
         BE    AOF60                                                            
         CLC   =C'ZZ',KEY+25                                                    
         BE    AOF75                                                            
         B     AOF60               SEQ TO NEXT                                  
         SPACE                                                                  
******                                                                          
* CODE FOR SCOPE FILTER                                                         
AOF75    DS    0H                                                               
         CLC   SCPFLTR,ALL         NO SCOPE FILTER?                             
         BE    AOF75AA             CHECK OFF FILTER                             
         CLI   SCPFLTR,C'B'        'BOTH' FILTER?                               
         BE    AOF75A              PRINT ALL                                    
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF60               NO, SKIP TO NEXT REC                         
*                                                                               
*** NEW CODE FOR OFFICE FILTER                                                  
AOF75AA  CLC   OFFLTR,ALL                                                       
         BE    AOF75A              NO OFFICE FILTER, DISPLAY                    
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF60               NO, SKIP TO NEXT RECORD                      
*                                                                               
******                                                                          
AOF75A   MVC   SAVEKEY,KEY         SAVE KEY OF CURRENT AGY REC                  
         LR    R0,R2                                                            
         LA    RF,3                AT LEAST 3 LINES LEFT ON SCREEN              
AOF76    CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         BAS   RE,CLRLINE                                                       
         ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)                                                      
         BCT   RF,AOF76                                                         
         LR    R2,R0                                                            
         USING LINEA,R2            FIRST LINE                                   
         ZIC   R5,0(R2)                                                         
         LA    R3,0(R5,R2)                                                      
         USING LINEB,R3            SECOND LINE                                  
         ZIC   R5,0(R3)                                                         
         LA    R4,0(R5,R3)                                                      
         USING LINEC,R4            THIRD LINE                                   
*                                                                               
AOF80    BAS   RE,GETREC                                                        
         CLC   =C'0000',RAGYKAGY   'CONTROL RECORD' ID?                         
         BNE   AOF81               NO                                           
         CLC   =C'CONTROL',RAGYNAM1                                             
*                                  CONTROL RECORD?                              
         BE    AOF60               YES - SKIP IT                                
AOF81    EQU   *                                                                
         CLI   OPT2BYTE,7          EI AGENCIES ONLY?                            
         BNE   AOF85               NO - ACCEPT ALL                              
         CLI   RAGYPRO1,C'Y'       EASI AGENCY?                                 
         BNE   AOF60               NO - SKIP IT                                 
*                                                                               
AOF85    DS    0H                                                               
         MVC   LAAGY,RAGYKAGY                                                   
         MVC   LAAOF,RAGYKAOF                                                   
         MVC   LANAM,RAGYNAM1                                                   
         MVC   LAADDR1(20),RAGYADD1                                             
         MVC   LCCITY(20),RAGYCITY                                              
         MVC   LCSTATE,RAGYSTAT                                                 
         MVC   LCZIP,RAGYZIP                                                    
         TM    RAGYFLAG,X'80'      EXPANDED ADDRESS?                            
         BZ    AOF92               NO - SKIP READING EXPANDED ADDRESS           
*                                                                               
         MVI   KEY,X'1A'           GET AGY2 RECORD WITH SAME KEY                
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETREC                                                        
         MVI   ELCODE,X'20'        EXPANDED ADDRESS ELEMENT                     
         LA    R6,RAGY2REC                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RAGY2AE1,R6                                                      
         MVC   LAADDR1,RAGY2AD1    WRITE EXPANDED ADDRESS FIELDS                
         MVC   LBADDR2,RAGY2AD2    TO SCREEN                                    
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SAVEKEY               RESTORE READ SEQ LOOP FOR              
         BAS   RE,HIGH                   AGENCY RECORDS                         
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   LCCITY,LBADDR2                                                   
         BNE   AOF92                                                            
         XC    LCCITY,LCCITY                                                    
*                                                                               
AOF92    DS    0H                                                               
         OC    LBADDR2,SPACES                                                   
         CLC   LBADDR2,SPACES      IS LINE 2 BLANK                              
         BNE   AOF93               NO                                           
         MVC   8(79,R3),8(R4)      YES - MOVE LINE 3 TO LINE 2                  
         XC    8(79,R4),8(R4)            AND CLEAR LINE 3                       
*                                                                               
AOF93    DS    0H                                                               
         FOUT  (R2)                XMIT LINE 1                                  
         FOUT  (R3)                XMIT LINE 2                                  
         FOUT  (R4)                XMIT LINE 3                                  
*                                                                               
         ZIC   R5,0(R4)                                                         
         LA    R2,0(R5,R4)         BUMP TO NEXT LINE                            
         CLI   0(R2),0             BOTTOM OF SCREEN?                            
         BE    AOF60               YES - DONE                                   
         OC    8(79,R4),8(R4)      IS LINE 3 BLANK?                             
         BZ    AOF60               YES - DONE                                   
         BAS   RE,CLRLINE          NO - CLEAR LINE 4                            
AOF95    FOUT  (R2)                XMIT LINE 4 (BLANK)                          
         ZIC   R5,0(R2)                                                         
         LA    R2,0(R5,R2)                                                      
         B     AOF60                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
AOF100   CLI   NEXTBYTE,1                                                       
         BE    AOF102                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),INFSTRT                                                
         OC    KEY+19(6),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
AOF102   BAS   RE,HIGH                                                          
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
         B     AOF120                                                           
         SPACE 1                                                                
AOF110   EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
*                                                                               
AOF120   CLC   AGYFLTR,ALL                                                      
         BE    AOF122                                                           
         CLC   AGYFLTR,KEY+19                                                   
         BNE   AOF110                                                           
*                                                                               
AOF122   CLI   REPBYTE,1                                                        
         BE    AOF124                                                           
         CLC   REPALPHA,KEY+25                                                  
         BE    AOF125                                                           
*                                                                               
AOF124   CLI   REPBYTE,2                                                        
         BE    AOF110                                                           
         CLC   =C'ZZ',KEY+25                                                    
         BE    AOF125                                                           
         B     AOF110              SEQ TO NEXT                                  
         SPACE                                                                  
******                                                                          
* CODE FOR SCOPE FILTER                                                         
AOF125   DS    0H                                                               
         CLC   SCPFLTR,ALL         NO SCOPE FILTER?                             
         BE    AOF125AA                                                         
         CLI   SCPFLTR,C'B'        'BOTH' FILTER?                               
         BE    AOF125A             PRINT ALL                                    
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF110              NO, SKIP TO NEXT REC                         
*                                                                               
*** NEW CODE FOR OFFICE FILTER                                                  
AOF125AA CLC   OFFLTR,ALL                                                       
         BE    AOF125A             NO OFFICE FILTER, DISPLAY                    
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF110              NO, SKIP TO NEXT RECORD                      
*                                                                               
******                                                                          
AOF125A  MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE19,R2                                                        
*                                                                               
AOF130   BAS   RE,GETREC                                                        
*                                                                               
AOF132   MVC   L19CODE,RAGYKAGY                                                 
         MVC   L19NAME,RAGYNAM2                                                 
         OC    RAGYRISK,RAGYRISK                                                
         BNZ   AOF135                                                           
         MVI   L19RISK,C'1'                                                     
         SR    RF,RF                                                            
         B     AOF138                                                           
*                                                                               
AOF135   EDIT  RAGYRISK,(1,L19RISK)  GET RISK DESCRIPTION                       
         ZIC   RF,RAGYRISK                                                      
         BCTR  RF,0                                                             
         MH    RF,=H'40'                                                        
AOF138   L     RE,=A(RISKTAB)                                                   
         A     RE,RELO1                                                         
         AR    RE,RF                                                            
         MVC   L19DESC,0(RE)                                                    
*                                                                               
AOF140   FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     AOF110                                                           
         DROP  R2                                                               
         EJECT                                                                  
AOF150   CLI   NEXTBYTE,1                                                       
         BE    AOF152                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),INFSTRT                                                
         OC    KEY+19(6),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
AOF152   BAS   RE,HIGH                                                          
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
         B     AOF170                                                           
*                                                                               
AOF160   EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
*                                                                               
AOF170   CLC   AGYFLTR,ALL                                                      
         BE    AOF172                                                           
         CLC   AGYFLTR,KEY+19                                                   
         BNE   AOF160                                                           
*                                                                               
AOF172   CLI   REPBYTE,1                                                        
         BE    AOF174                                                           
         CLC   REPALPHA,KEY+25                                                  
         BE    AOF175                                                           
*                                                                               
AOF174   CLI   REPBYTE,2                                                        
         BE    AOF160                                                           
         CLC   =C'ZZ',KEY+25                                                    
         BE    AOF175                                                           
         B     AOF160              SEQ TO NEXT                                  
         SPACE                                                                  
******                                                                          
* CODE FOR SCOPE FILTER                                                         
AOF175   DS    0H                                                               
         CLC   SCPFLTR,ALL         NO SCOPE FILTER?                             
         BE    AOF175AA                                                         
         CLI   SCPFLTR,C'B'        'BOTH' FILTER?                               
         BE    AOF175A                                                          
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF160              NO, SKIP TO NEXT REC                         
*                                                                               
*** NEW CODE FOR OFFICE FILTER                                                  
AOF175AA CLC   OFFLTR,ALL                                                       
         BE    AOF175A             NO OFFICE FILTER, DISPLAY                    
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF160              NO, SKIP TO NEXT RECORD                      
*                                                                               
******                                                                          
AOF175A  MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE20,R2                                                        
*                                                                               
AOF180   BAS   RE,GETREC                                                        
*                                                                               
AOF182   MVC   L20CODE,RAGYKAGY                                                 
         MVC   L20NAME,RAGYNAM2                                                 
         OC    RAGYLIAB,RAGYLIAB                                                
         BZ    AOF190                                                           
*                                                                               
AOF185   EDIT  RAGYLIAB,(2,L20LIAB),FILL=0  GET LIAB POS                        
*                                                                               
         XC    KEY,KEY             GET LIAB POS COMMENT REC                     
         MVI   KEY,X'2E'                                                        
         MVC   KEY+15(2),REPALPHA                                               
         OC    KEY+17(2),=X'FFFF'                                               
         MVC   KEY+19(8),=C'LIAB    '                                           
         MVC   KEY+23(2),L20LIAB                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AOF188                                                           
         BAS   RE,GETREC                                                        
*                                                                               
         L     R6,AIOAREA                                                       
         USING RCMTELM2,R6         COMMENT TEXT ELEMENT                         
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   AOF188                                                           
*                                                                               
AOF186   DS    0H                                                               
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    AOF187                                                           
         CLI   RCMT2TXT,C' '                                                    
         BNE   AOF187                                                           
         MVI   BYTE,2              COMMENT TEXT ELEMENT                         
         BAS   RE,NEXTEL           R4 HAS ADDRESS OF FIRST ELEMENT              
         BE    AOF186                                                           
         B     AOF188                                                           
*                                                                               
AOF187   DS    0H                                                               
         CLI   RCMT2LEN,37                                                      
         BH    AOF187A                                                          
         ZIC   R1,RCMT2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     AOF188                                                           
         MVC   L20DESC(0),RCMT2TXT                                              
*                                                                               
AOF187A  MVC   L20DESC,RCMT2TXT                                                 
         DROP  R6                                                               
*                                                                               
AOF188   MVC   KEY(27),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
AOF190   FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     AOF160                                                           
         DROP  R2                                                               
         EJECT                                                                  
AOF200   CLI   NEXTBYTE,1                                                       
         BE    AOF202                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),INFSTRT                                                
         OC    KEY+19(6),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
AOF202   BAS   RE,HIGH                                                          
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
         B     AOF220                                                           
*                                                                               
AOF210   EQU   *                                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,SEQ                                                           
         CLI   KEY,X'0A'                                                        
         BNE   SETDONE                                                          
*                                                                               
AOF220   CLC   AGYFLTR,ALL                                                      
         BE    AOF222                                                           
         CLC   AGYFLTR,KEY+19                                                   
         BNE   AOF210                                                           
*                                                                               
AOF222   CLI   REPBYTE,1                                                        
         BE    AOF224                                                           
         CLC   REPALPHA,KEY+25                                                  
         BE    AOF225                                                           
*                                                                               
AOF224   CLI   REPBYTE,2                                                        
         BE    AOF210                                                           
         CLC   =C'ZZ',KEY+25                                                    
         BE    AOF225                                                           
         B     AOF210              SEQ TO NEXT                                  
         SPACE                                                                  
******                                                                          
* CODE FOR SCOPE FILTER                                                         
AOF225   DS    0H                                                               
         CLC   SCPFLTR,ALL         NO SCOPE FILTER?                             
         BE    AOF225AA            PRINT ALL                                    
         CLI   SCPFLTR,C'B'        'BOTH' FILTER?                               
         BE    AOF225A                                                          
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF210              NO, SKIP TO NEXT REC                         
*                                                                               
*** NEW CODE FOR OFFICE FILTER                                                  
AOF225AA CLC   OFFLTR,ALL                                                       
         BE    AOF225A             NO OFFICE FILTER, DISPLAY                    
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AOF210              NO, SKIP TO NEXT RECORD                      
*                                                                               
******                                                                          
AOF225A  MVC   SAVEKEY,KEY                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         USING LINE21,R2                                                        
*                                                                               
AOF230   BAS   RE,GETREC                                                        
*                                                                               
AOF232   MVC   L21CODE,RAGYKAGY                                                 
         MVC   L21NAME,RAGYNAM2                                                 
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVI   KEY,RAGK2TYQ                                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AOF238                                                           
*                                                                               
         BAS   RE,GETREC                                                        
         L     R6,AIOAREA                                                       
         USING RAGY2FXE,R6                                                      
         MVI   ELCODE,RAGY2CDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   AOF238                                                           
*                                                                               
         OC    RAGY2FON,RAGY2FON   PHONE NUMBER                                 
         BZ    AOF235                                                           
         MVI   L21PHONE,C'('                                                    
         MVC   L21PHONE+1(3),RAGY2FON                                           
         MVI   L21PHONE+4,C')'                                                  
         MVC   L21PHONE+5(3),RAGY2FON+3                                         
         MVI   L21PHONE+8,C'-'                                                  
         MVC   L21PHONE+9(4),RAGY2FON+6                                         
*                                                                               
AOF235   DS    0H                                                               
         OC    RAGY2FAX,RAGY2FAX   FAX NUMBER                                   
         BZ    AOF238                                                           
         MVI   L21FAX,C'('                                                      
         MVC   L21FAX+1(3),RAGY2FAX                                             
         MVI   L21FAX+4,C')'                                                    
         MVC   L21FAX+5(3),RAGY2FAX+3                                           
         MVI   L21FAX+8,C'-'                                                    
         MVC   L21FAX+9(4),RAGY2FAX+6                                           
         DROP  R6                                                               
*                                                                               
AOF238   MVC   KEY(27),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
AOF240   FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     AOF210                                                           
         DROP  R2                                                               
         EJECT                                                                  
AOF300   DS    0H                  TERRITORY PASSIVE KEY                        
         XC    SAVEKEY,SAVEKEY                                                  
         ZAP   DUB,=P'0'                                                        
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         CLI   NEXTBYTE,1                                                       
         BE    AOF302                                                           
*                                                                               
         XC    KEY,KEY                                                          
K        USING RAGY2REC,KEY                                                     
         MVI   KEY,X'AA'                                                        
         MVC   K.RAGKTREP,REPALPHA                                              
         MVC   K.RAGKTTER,INFSTRT                                               
         OC    K.RAGKTTER,SPACES                                                
*                                                                               
AOF302   BAS   RE,HIGH             FIRST READ                                   
         CLI   KEY,X'AA'           SAME REC TYPE                                
         BNE   NOREC                                                            
         CLC   K.RAGKTREP,REPALPHA                                              
         BNE   NOREC                                                            
         B     AOF320                                                           
*                                                                               
AOF310   BAS   RE,SEQ                                                           
         CLI   KEY,X'AA'           SAME REC TYPE                                
         BNE   *+14                                                             
         CLC   K.RAGKTREP,REPALPHA                                              
         BE    AOF320                                                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    SETDONE                                                          
*                                                                               
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
AOF312   DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RF,RE                                                            
         BCT   R1,AOF312                                                        
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
AOF314   DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,AOF314                                                        
*                                                                               
         B     SETDONE                                                          
*                                                                               
AOF320   DS    0H                                                               
         CLC   K.RAGKTTER,SAVEKEY+(RAGKTTER-RAGKTKEY)                           
         BE    AOF330               SAME TERRITORY AS LAST KEY                  
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    AOF328                                                           
*                                                                               
AOF322   DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CVB   R1,DUB                                                           
         LA    R1,1(R1)            LEAVE A LINE AT THE BOTTOM                   
         LR    RE,R2                                                            
AOF324   DS    0H                  VERIFY ENOUGH LINES FOR ENTRY                
         CLI   0(RE),0                                                          
         BE    SETNEXT                                                          
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         BCT   R1,AOF324                                                        
*                                                                               
         CVB   R1,DUB                                                           
         L     RE,AIOAREA                                                       
AOF326   DS    0H                  PRINT OUT STATION LIST LINES                 
         MVC   8(79,R2),0(RE)                                                   
         OI    6(R2),X'80'                                                      
         LA    RE,79(RE)                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,AOF326                                                        
*                                                                               
AOF328   DS    0H                                                               
         MVC   SAVEKEY,KEY         SAVE KEY WITH NEW MARKET                     
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   SETNEXT             NEXT PAGE                                    
*                                                                               
         L     RE,AIOAREA          CLEAR LINE BUFFER                            
         LA    RF,79*15                                                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   DUB,=P'1'           SET TO SINGLE LINE                           
         ZAP   HALF,=P'0'                                                       
         MVC   FULL,AIOAREA                                                     
*                                                                               
         L     R5,AIOAREA                                                       
         MVC   3(2,R5),K.RAGKTTER                                               
         LA    R5,12(R5)                                                        
*                                                                               
AOF330   DS    0H                                                               
         L     RE,FULL                                                          
         LR    RF,R5                                                            
         SR    RF,RE                                                            
         CH    RF,=H'70'                                                        
         BNH   AOF332                                                           
*                                                                               
         CP    DUB,=P'14'          MAX LINES ON SCREEN                          
         BNL   AOF322              PRINT IT OUT, ITS TOO BIG                    
*                                                                               
         LA    RE,79               DISPLACE TO NEXT LINE                        
         A     RE,FULL                                                          
         ST    RE,FULL             STORE START OF NEW LINE                      
         LA    R5,15(RE)                                                        
         AP    DUB,=P'1'           ADD ONE MORE LINE TO BUFFER                  
*                                                                               
AOF332   DS    0H                                                               
         AP    HALF,=P'1'          ADD ANOTHER STATION TO COUNT                 
*                                                                               
         MVC   0(4,R5),K.RAGKTAGY                                               
         LA    R5,4(R5)                                                         
*                                                                               
         CLC   K.RAGKTAOF,SPACES                                                
         BNH   AOF334                                                           
*                                                                               
         MVI   0(R5),C'-'                                                       
         MVC   1(2,R5),K.RAGKTAOF                                               
         LA    R5,3(R5)                                                         
*                                                                               
AOF334   DS    0H                                                               
         LA    R5,1(R5)                                                         
         B     AOF310               READ NEXT                                   
         DROP  K                                                                
*                                                                               
AOF400   DS    0H                  TERRITORY PASSIVE KEY                        
*** NEW                                                                         
         XC    TMPTER,TMPTER                                                    
         XC    TMPTER,TMPAGY                                                    
         XC    TMPTER,TMPAOF                                                    
         XC    TMPTER,TMPNAM                                                    
         LA    R4,AOF490                                                        
*                                                                               
         CLI   NEXTBYTE,1                                                       
         BE    AOF402                                                           
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RAGY2KEY,R6                                                      
         MVI   KEY,X'AA'                                                        
         MVC   RAGKTREP,REPALPHA                                                
         MVC   RAGKTTER,INFSTRT                                                 
         OC    RAGKTTER,SPACES                                                  
*                                                                               
AOF402   DS    0H                                                               
         LA    R6,KEY                                                           
         BAS   RE,HIGH             FIRST READ                                   
         CLI   KEY,X'AA'           SAME REC TYPE                                
         BNE   NOREC                                                            
         CLC   RAGKTREP,REPALPHA                                                
         BNE   NOREC                                                            
         B     AOF420                                                           
*                                                                               
AOF410   DS    0H                                                               
         LA    R6,KEY                                                           
         BAS   RE,SEQ                                                           
         CLI   KEY,X'AA'           SAME REC TYPE                                
         BNE   SETDONE                                                          
         CLC   RAGKTREP,REPALPHA                                                
         BNE   SETDONE                                                          
*                                                                               
AOF420   DS    0H                                                               
         MVC   SAVEKEY,KEY         STORE AGY2 KEY                               
*                                                                               
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
*                                                                               
         MVC   TMPTER,RAGKTTER     PUT VALUES FROM AGYREC2 IN TEMP              
         MVC   TMPAGY,RAGKTAGY                                                  
         MVC   TMPAOF,RAGKTAOF                                                  
*                                                                               
* GET AGY REC FOR AGY NAME                                                      
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         USING RAGYKEY,R6                                                       
         MVI   KEY,X'0A'                                                        
         MVC   RAGYKAGY,TMPAGY                                                  
         MVC   RAGYKAOF,TMPAOF                                                  
         MVC   RAGYKREP,REPALPHA                                                
         DROP  R6                                                               
*                                                                               
         BAS   RE,HIGH                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,GETREC                                                        
         LA    R6,RAGYREC                                                       
         MVI   ELCODE,1            GET FIRST ELEM                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RAGYELEM,R6                                                      
         MVC   TMPNAM,RAGYNAM1                                                  
*                                                                               
         MVC   KEY,SAVEKEY         RESTORE AGY2 KEY                             
         BAS   RE,HIGH             RESTART ORIGINAL KEY SEQUENCE                
*                                                                               
         USING LINEE,R2                                                         
         DROP  R6                                                               
         BR    R4                                                               
*                                                                               
AOF490   MVC   LETER,TMPTER                                                     
         MVC   LEAGY,TMPAGY                                                     
         MVC   LEAOF,TMPAOF                                                     
         MVC   LENAM,TMPNAM                                                     
         LA    R4,AOF500                                                        
         B     AOF410                                                           
*                                                                               
AOF500   MVC   LETER2,TMPTER                                                    
         MVC   LEAGY2,TMPAGY                                                    
         MVC   LEAOF2,TMPAOF                                                    
         MVC   LENAM2,TMPNAM                                                    
         LA    R4,AOF490                                                        
*                                                                               
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     AOF410               READ NEXT                                   
         DROP  R2                                                               
*                                                                               
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
SETDONE  FOUT  (R2)                                                             
         OC    INFOUT,INFOUT                                                    
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
*                                                                               
* CHECK SCOPE FILTER FOR DISPLAYING ADV RECS                                    
*                                                                               
CHKSCP   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,RAGK2TYQ        GET AGY2 RECORD                              
         MVC   KEY+19(4),SAVEKEY+19 AGENCY CODE                                 
         MVC   KEY+23(2),SAVEKEY+23 AGYOF CODE                                  
         MVC   KEY+25(2),SAVEKEY+25 REP CODE                                    
         OC    KEY+19(8),SPACES                                                 
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CSNX                NO SCOPE INFO, IS NATIONAL                   
*                                                                               
         BAS   RE,GETREC                                                        
         L     R6,AIOAREA                                                       
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
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         BAS   RE,HIGH                                                          
         B     CSX                 DON'T DISPLAY                                
*                                                                               
CSDX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         BAS   RE,HIGH                                                          
CSX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK OFFICE FILTER FOR DISPLAYING ADV RECS                                   
*                                                                               
CHKOFF   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,RAGK2TYQ        GET AGY2 RECORD                              
         MVC   KEY+19(4),SAVEKEY+19 AGENCY CODE                                 
         MVC   KEY+23(2),SAVEKEY+23 AGYOF CODE                                  
         MVC   KEY+25(2),SAVEKEY+25 REP CODE                                    
         OC    KEY+19(8),SPACES                                                 
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   COSX                NO SCOPE INFO, SKIP                          
*                                                                               
         BAS   RE,GETREC                                                        
         L     R6,AIOAREA                                                       
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
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         BAS   RE,HIGH                                                          
         B     COX                                                              
*                                                                               
CODX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         BAS   RE,HIGH                                                          
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
*******************************                                                 
*CLEAR KEYTABLE IN REINFWRK                                                     
*                                                                               
CLRKTAB  NTR1                                                                   
         LHI   R3,NUMENTSK                                                      
         LA    R4,KEYTAB                                                        
CLR10    XC    0(L'KEYTAB,R4),0(R4)                                             
         LA    R4,L'KEYTAB(R4)                                                  
         BCT   R3,CLR10                                                         
         XIT1                                                                   
*                                                                               
*******************************                                                 
*BUILD AGENCY KEY -R7 MUST POINT TO KEYTAB ENTRY                                
*                                                                               
BLDAGYK  NTR1                                                                   
         MVC   0(4,R7),KEY+19      INSERT AGENCY CODE                           
*                                                                               
         LHI   R1,4                LIMIT COUNTER(SAFETY)                        
BLDAGY10 CLI   0(R7),X'40'         LOOK FOR SPACE                               
         BE    BLDAGY20            IF FOUND,CHECK FOR TERR                      
         LA    R7,1(R7)            ELSE, BUMP TO NEXT CHAR                      
         BCT   R1,BLDAGY10         AND REPEAT LOOP                              
*                                                                               
BLDAGY20 CLI   KEY+23,X'40'        TERRITORY CODE PRES?                         
         BNE   BLDAGY30            IF YES, GRAB IT                              
         MVI   0(R7),X'00'         ELSE, PLACE A NULL AT END                    
         B     BLDAGYX              FOR FILE PROG COMPLIANCE                    
*                                   AND EXIT                                    
*                                                                               
BLDAGY30 MVI   0(R7),C'-'          INSERT HYPHEN                                
         MVC   1(2,R7),KEY+23      AND APPEND TERR. CODE                        
BLDAGYX  XIT1                                                                   
*                                                                               
*******************************                                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
MSG1     DC    C'ACTION COMPLETED - ENTER NEXT REQUEST'                         
MSG2     DC    C'MORE RECORDS AVAILABLE - HIT ''ENTER'' TO CONTINUE'            
MSG3     DC    C'NO RECORDS ON FILE - ENTER NEXT REQUEST'                       
SPACES   DC    CL80' '                                                          
ALL      DC    5X'00'                                                           
ELCODE   DC    1X'00'                                                           
*                                                                               
TMPTER   DS    CL2                 TEMPORARY TERRITORY STORAGE                  
TMPAGY   DS    CL4                 TEMPORARY AGENCY STORAGE                     
TMPAOF   DS    CL2                 TEMPORARY AGYOF STORAGE                      
TMPNAM   DS    CL20                TEMP AGY NAME                                
*                                                                               
RTNFLG   DS    CL1                                                              
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
       ++INCLUDE RERISKTAB                                                      
       ++INCLUDE REINFWRKA                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
LINEE    DSECT                                                                  
         DS    CL8                                                              
LETER    DS    CL2                 LINE E TERRITORY CODE                        
         DS    CL4                                                              
LEAGY    DS    CL4                                                              
         DS    CL1                                                              
LEAOF    DS    CL2                                                              
         DS    CL2                                                              
LENAM    DS    CL20                                                             
         DS    CL6                                                              
*                                                                               
LETER2   DS    CL2                 LINE E TERRITORY CODE                        
         DS    CL4                                                              
LEAGY2   DS    CL4                                                              
         DS    CL1                                                              
LEAOF2   DS    CL2                                                              
         DS    CL2                                                              
LENAM2   DS    CL20                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'255REINF03S  05/01/02'                                      
         END                                                                    
