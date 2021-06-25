*          DATA SET ACLFM2B    AT LEVEL 083 AS OF 05/01/02                      
*PHASE T6032BA,*                                                                
ACLFM2B  TITLE '- OFFICE/OFFICE LIST RECORDS FOR ACCOUNT FILE'                  
T6032B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FM2B**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA          RA=A(TWA)                                    
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC         RC=A(GLOBAL W/S)                             
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         MVC   VHELLO,CHELLO                                                    
         MVC   VGETTXT,CGETTXT                                                  
         DROP  RF                                                               
*                                                                               
         CLI   MODE,BUILDKEY                                                    
         BE    OFLKEY                                                           
         BAS   RE,SCRINIT                                                       
         BAS   RE,SVSCR            SAVE CURRENT SCREEN                          
         BAS   RE,VALPF            VALIDATE PF KEYS                             
         CLI   ERROR,X'FF'         DID WE RETURN WITH AN ERROR?                 
         BNE   CSXIT                   YES ERROR OUT                            
         CLI   MODE,DSPLYREC                                                    
         BE    DISREC                                                           
         CLI   MODE,BUILDREC                                                    
         BE    OFLBLD                                                           
         DC    H'0'                                                             
*                                                                               
OKXIT    MVI   ERROR,X'FF'                                                      
CSXIT    OI    6(R2),X'40'         SET CURSOR                                   
         XIT1  REGS=(R2)                                                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE KEY                                                       *          
**********************************************************************          
         SPACE 1                                                                
OFLKEY   DS    0H                                                               
         CLC   LSTACT,LOGACT       SAME ACTION AS BEFORE?                       
         BNE   *+14                NO-REINITIALIZE                              
         CLC   LSTOFF,LOGOLC       SAME CODE AS BEFORE?                         
         BE    OFLK10                                                           
         MVI   OFFDSP,0            START AT BEGINNING OF OFFICE LIST            
         MVI   CURSCR,FSTSCR       ALWAYS LOAD FIRST SCREEN FIRST               
         MVC   LSTOFF,LOGOLC       UPDATE LAST OFFICE                           
         MVC   LSTACT,LOGACT       SAVE ACTION FOR LATER COMPARE                
         XC    SVOFF,SVOFF         SAVED AREA FOR OFFICE                        
         XC    SVOFFPV,SVOFFPV     SAVED AREA FOR OFFICE-PREV SCR               
         XC    OFFNUM,OFFNUM       NUMBER OF ENTRIES IN OFF LIST                
         TWAXC LOGOC0AH,LOGOC0LH,PROT=Y                                         
*                                                                               
OFLK10   LA    R3,IO                                                            
         USING OFFRECD,R3                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         LA    R2,LOGOLCH                                                       
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    ERRMISS                                                          
         CH    R1,=H'2'            TEST OFF/OFF LIST CODE = 2 BYTES             
         BL    ERRSHORT                                                         
         MVC   OFFKOFF,LOGOLC                                                   
         OC    OFFKOFF,SPACES                                                   
*                                                                               
         SR    R0,R0                                                            
         CLI   LOGACT,C'E'                                                      
         BE    *+8                                                              
         CLI   LOGACT,C'I'                                                      
         BE    *+8                                                              
         LA    R0,X'80'            SET READ FOR UPDATE                          
         GOTO1 DATAMGR,DMCB,((R0),DMREAD),ACCFIL,OFFRECD,OFFRECD                
         BE    *+16                                                             
         TM    8(R1),X'90'         TEST E-O-F/NOT FOUND                         
         BZ    ERRDSK                                                           
         B     OFLK20                                                           
         CLI   LOGACT,C'N'         CAN'T BE NEW                                 
         BE    ERRFND              ERROR - RECORD FOUND                         
         B     OFLK30                                                           
OFLK20   CLI   LOGACT,C'N'         MUST BE NEW                                  
         BNE   ERRNFND             ERROR - RECORD NOT FOUND                     
         MVI   ANYKEY,C'N'                                                      
         B     OFLKX                                                            
*                                                                               
OFLK30   TM    LOGOLCH+4,X'20'                                                  
         BZ    *+12                                                             
         TM    LOGACTH+4,X'20'                                                  
         BNZ   OFLKX                                                            
         MVI   ANYKEY,C'Y'                                                      
         OI    LOGOLCH+4,X'20'                                                  
         OI    LOGACTH+4,X'20'                                                  
*                                                                               
OFLKX    LA    R2,LOGOLCH                                                       
         B     OKXIT                                                            
         DROP  R3                                                               
         EJECT                                                                  
******************************************************************              
* DISPLAY OFFICE DETAILS/OFFICE LIST                             *              
******************************************************************              
         SPACE 1                                                                
DISREC   DS    0H                                                               
         CLI   PFKEY,0             DO NOT DISPLAY FOR PFKEYS                    
         BNE   DISRX                                                            
         OC    SVOFF,SVOFF         ANYTHING IN SVOFF?                           
         BNZ   DISRX                                                            
         TWAXC LOGOC0AH,LOGOC0LH,PROT=Y                                         
*                                                                               
         XC    LOGOLLN,LOGOLLN                                                  
         OI    LOGOLLNH+6,X'80'                                                 
         XC    LOGOLSN,LOGOLSN                                                  
         OI    LOGOLSNH+6,X'80'                                                 
         LA    R2,LOGOC0AH                                                      
*                                                                               
         LA    R3,IO                                                            
         USING OFFRECD,R3                                                       
         LA    R1,OFFRECD+(ACRECORD-ACKEYD)                                     
         SR    R0,R0                                                            
DISR10   CLI   0(R1),0                                                          
         BE    DISR50                                                           
         CLI   0(R1),NAMELQ                                                     
         BE    DISR30                                                           
         CLI   0(R1),SNMELQ                                                     
         BE    DISR40                                                           
DISR20   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DISR10                                                           
*                                                                               
         USING NAMELD,R1                                                        
DISR30   SR    RE,RE               DISPLAY LONG NAME                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   LOGOLLN(0),NAMEREC                                               
         B     DISR20                                                           
*                                                                               
         USING SNMELD,R1                                                        
DISR40   MVC   LOGOLSN,SNMNAME     DISPLAY SHORT NAME                           
         B     DISR20                                                           
*                                                                               
DISR50   LA    R1,OFFRECD+(ACRECORD-ACKEYD)                                     
         USING OFIELD,R1           LOCATE OFFICE INFO ELEMENT                   
DISR60   CLI   OFIEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   OFIEL,OFIELQ                                                     
         BE    *+14                                                             
         IC    R0,OFILN                                                         
         AR    R1,R0                                                            
         B     DISR60                                                           
         TM    OFISTAT,OFISLIST    IF NOT A LIST                                
         BZ    DISRX               CLEAR SCREEN AND EXIT                        
*                                                                               
         USING OFLELD,R1                                                        
DISR70   CLI   OFLEL,0             LOOK FOR LIST ELEMENT                        
         BE    DISRX                                                            
         CLI   OFLEL,OFLELQ                                                     
         BE    *+14                                                             
         IC    R0,OFLLN                                                         
         AR    R1,R0                                                            
         B     DISR70                                                           
*                                                                               
         SR    R5,R5                                                            
         IC    R5,OFLLN            CALC NO. OF CODES IN ELEMENT                 
         SH    R5,=Y(OFLLN1Q+1)                                                 
         BP    *+6                                                              
         DC    H'0'                                                             
         EX    R5,*+4                                                           
         MVC   SVOFF(0),OFLNTRY    SET OFF LIST WITH ELM                        
         LA    R5,1(R5)            RESET ORIGINAL LENGTH                        
         SRA   R5,1                                                             
         LTR   R5,R5                                                            
         BZ    DISRX                                                            
         STC   R5,OFFNUM                                                        
*                                                                               
         LA    R4,OFLNTRY                                                       
         USING OFLNTRY,R4                                                       
DISR80   MVC   8(L'OFLNTRY,R2),OFLNTRY                                          
         OI    6(R2),X'80'                                                      
         LA    R3,IO2                                                           
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,OFLNTRY                                                  
         OC    OFFKOFF,SPACES                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,OFFRECD,OFFRECD                       
         BE    *+6                                                              
         DC    H'0'                OFFICE RECORD DOESN'T EXIST                  
         LA    R1,OFFRECD+(ACRECORD-ACKEYD)                                     
         SR    R0,R0                                                            
         USING SNMELD,R1                                                        
DISR90   CLI   SNMEL,0             LOOK FOR SHORT NAME ELEMENT                  
         BE    DISRX                                                            
         CLI   SNMEL,SNMELQ                                                     
         BE    *+14                                                             
         IC    R0,SNMLN                                                         
         AR    R1,R0                                                            
         B     DISR90                                                           
*                                                                               
         L     R2,ANXTCLR                                                       
         MVC   8(L'SNMNAME,R2),SNMNAME                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,NXTENT                                                        
         BNE   DISRX                                                            
         LA    R4,L'OFLNTRY(R4)    NEXT OFFICE CODE                             
         L     R2,ANXTCLL                                                       
         BCT   R5,DISR80                                                        
*                                                                               
DISRX    LA    R2,LOGOLCH                                                       
         B     OKXIT                                                            
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* BUILD OR AMEND OFFICE/OFFICE LIST RECORD                           *          
**********************************************************************          
         SPACE 1                                                                
OFLBLD   DS    0H                                                               
         CLI   PFKEY,0                                                          
         BE    *+12                DO NOT UPDATE IF PFKEY                       
         OI    BLDFLG,OFFLSTQ                                                   
         B     OFLB240                                                          
*                                                                               
         XC    BLDFLG,BLDFLG                                                    
         XC    ERROR,ERROR                                                      
         XC    DELOFL,DELOFL                                                    
         XC    ADDOFL,ADDOFL                                                    
OFLB210  LA    R3,IO2                                                           
         USING OFFRECD,R3                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         L     R2,ANXTCLL          OFFICE CODE                                  
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    OFLB230                                                          
         CLC   8(2,R2),SPACES      MAKE SURE SIGNIFICANT INFO                   
         BNH   OFLB230                                                          
*                                                                               
         MVC   OFFKOFF,8(R2)                                                    
         OC    OFFKOFF,SPACES                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,OFFRECD,OFFRECD                       
         BNE   ERRNFND             OFFICE REC DOESN'T EXIST                     
         TM    OFFRECD+(ACSTATUS-ACKEYD),OFFSLIST                               
         BNZ   ERRLST              OFFICE REC IS A LIST                         
         OI    BLDFLG,OFFLSTQ                                                   
         L     R2,ANXTCLR                                                       
         SR    R0,R0                                                            
         LA    R1,OFFRECD+(ACRECORD-ACKEYD)                                     
         USING SNMELD,R1           LOCATE SHORT NAME ELEMENT                    
OFLB220  CLI   SNMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SNMEL,SNMELQ                                                     
         BE    *+14                                                             
         IC    R0,SNMLN                                                         
         AR    R1,R0                                                            
         B     OFLB220                                                          
*                                                                               
         MVC   8(L'SNMNAME,R2),SNMNAME                                          
         OI    6(R2),X'80'                                                      
OFLB230  BAS   RE,NXTENT                                                        
         BE    OFLB210                                                          
OFLB240  CLI   LOGACT,C'N'                                                      
         BNE   OFLB10                                                           
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE NEW RECORD                                                   *         
***********************************************************************         
         SPACE 1                                                                
OFLB4    LA    R3,IO                                                            
         USING OFFRECD,R3                                                       
         XC    OFFRECD(256),OFFRECD                                             
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ    BUILD NEW RECORD                             
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,LOGOLC                                                   
         OC    OFFKOFF,SPACES                                                   
*                                                                               
         LA    R2,LOGOLCH                                                       
         GOTO1 TALPHA,OFFKOFF                                                   
*                                                                               
         TM    BLDFLG,OFFLSTQ                                                   
         BZ    *+8                                                              
         OI    OFFRECD+(ACSTATUS-ACKEYD),OFFSLIST                               
         MVC   SAVSTAT,OFFRECD+(ACSTATUS-ACKEYD)                                
         SR    R0,R0                                                            
         LA    R1,OFFRECD+(ACRECORD-ACKEYD)                                     
         BAS   RE,BLNEL            LONG NAME ELEMENT                            
         BAS   RE,BSNEL            SHORT NAME ELEMENT                           
         BAS   RE,BOIEL            OFFICE INFO ELEMENT                          
         TM    BLDFLG,OFFLSTQ                                                   
         BZ    OFLB6                                                            
         BAS   RE,BOLEL            LIST ELEMENT                                 
         BNE   ERRDUP              DUPLICATED CODE IN LIST                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
OFLB6    MVI   0(R1),0             SET END OF RECORD                            
         LA    R1,1(R1)                                                         
         LA    R0,OFFRECD                                                       
         SR    R1,R0                                                            
         STCM  R1,3,OFFRECD+(ACLENGTH-ACKEYD)                                   
         GOTO1 UPREC,DMCB,('UPADD',OFFRECD)                                     
         B     OFLB50                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* AMEND EXISTING RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
OFLB10   LA    R3,IO                                                            
         USING OFFRECD,R3                                                       
         MVC   SAVSTAT,OFFRECD+(ACSTATUS-ACKEYD)                                
         XC    OFISV,OFISV                                                      
*                                                                               
         LA    R4,OFFRECD+(ACRECORD-ACKEYD)                                     
OFLB12   CLI   0(R4),0                                                          
         BE    OFLB20                                                           
         CLI   0(R4),NAMELQ                                                     
         BE    OFLB16                                                           
         CLI   0(R4),SNMELQ                                                     
         BE    OFLB18                                                           
         CLI   0(R4),OFIELQ                                                     
         BE    OFLB19                                                           
         CLI   0(R4),OFLELQ                                                     
         BE    OFLB22                                                           
OFLB14   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     OFLB12                                                           
*                                                                               
OFLB16   GOTO1 HELDEL,DMCB,('NAMELQ',OFFRECD)                                   
         GOTO1 BLNEL,ELEMENT                                                    
         GOTO1 HELADD,DMCB,OFFRECD,ELEMENT                                      
         B     OFLB14                                                           
*                                                                               
OFLB18   GOTO1 HELDEL,DMCB,('SNMELQ',OFFRECD)                                   
         GOTO1 BSNEL,ELEMENT                                                    
         GOTO1 HELADD,DMCB,OFFRECD,ELEMENT                                      
         B     OFLB14                                                           
*                                                                               
         USING OFIELD,R4                                                        
OFLB19   MVC   OFISV,OFIELD        SAVE OFFICE INFO ELEMENT                     
         B     OFLB14                                                           
*                                                                               
OFLB20   TM    BLDFLG,OFFLSTQ      IF NEW OFFICE LIST                           
         BZ    OFLB26                                                           
         OC    OFISV+(OFIINC-OFIELD)(L'OFIINC),OFISV+(OFIINC-OFIELD)            
         BZ    OFLB24              CREATE NEW ELEMENT                           
         LA    R2,LOGOLCH                                                       
         B     ERRMEM                                                           
*                                                                               
         USING OFLELD,R4                                                        
OFLB22   LA    R2,LOGOC0AH         UPDATE OFFICE LIST                           
         XC    DELOFL,DELOFL                                                    
         XC    ADDOFL,ADDOFL                                                    
         OC    OFISV+(OFIUSE-OFIELD)(L'OFIUSE),OFISV+(OFIUSE-OFIELD)            
         BZ    *+12                                                             
         TM    BLDFLG,OFFLSTQ                                                   
         BZ    ERRCDEL             CAN'T NOW BE DELETED                         
         SR    RE,RE                                                            
         IC    RE,OFLLN                                                         
         SH    RE,=Y(OFLLN1Q+1)                                                 
         BNP   OFLB23                                                           
         EX    RE,*+4                                                           
         MVC   DELOFL(0),OFLNTRY   SAVE IT TO COMPARE WITH NEW ONE              
*                                                                               
OFLB23   GOTO1 HELDEL,DMCB,('OFLELQ',OFFRECD)                                   
         TM    BLDFLG,OFFLSTQ      TEST IF NEW ONE REQUIRED                     
         BNZ   OFLB24                                                           
         NI    OFFRECD+(ACSTATUS-ACKEYD),X'FF'-OFFSLIST                         
*                                                                               
         LA    R4,OFFRECD+(ACRECORD-ACKEYD)                                     
         SR    R0,R0                                                            
         USING OFIELD,R4                                                        
OFLB23A  CLI   OFIEL,0             TURN BIT OFF IN INFO ELEMENT ALSO            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   OFIEL,OFIELQ                                                     
         BE    *+14                                                             
         IC    R0,OFILN                                                         
         AR    R4,R0                                                            
         B     OFLB23A                                                          
*                                                                               
         NI    OFISTAT,X'FF'-OFISLIST                                           
         B     OFLB26                                                           
         DROP  R4                                                               
*                                                                               
OFLB24   GOTO1 BOLEL,ELEMENT       BUILD NEW LIST ELEMENT                       
         BNE   ERRDUP              DUPLICATE OFFICE CODE IN LIST                
         OC    OFISV+(OFIUSE-OFIELD)(L'OFIUSE),OFISV+(OFIUSE-OFIELD)            
         BZ    *+14                                                             
         OC    DELOFL,DELOFL       ENSURE OLD CODES ARE IN NEW LIST             
         BNZ   ERRCDEL             CODE(S) HAVE BEEN DROPPED                    
         GOTO1 HELADD,DMCB,OFFRECD,ELEMENT                                      
         OI    OFFRECD+(ACSTATUS-ACKEYD),OFFSLIST                               
         OI    OFISV+(OFISTAT-OFIELD),OFISLIST                                  
         GOTO1 HELDEL,DMCB,('OFIELQ',OFFRECD)                                   
         GOTO1 HELADD,DMCB,OFFRECD,OFISV                                        
         NI    BLDFLG,X'FF'-OFFLSTQ                                             
         B     OFLB26                                                           
*                                  WRITE BACK DATA REC AND POINTER(S)           
OFLB26   GOTO1 UPREC,DMCB,('UPPUT',OFFRECD)                                     
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL LIST AND EX-LIST CODES                                   *         
***********************************************************************         
         SPACE 1                                                                
OFLB50   LA    R3,IO                                                            
         USING OFFRECD,R3                                                       
         XC    HALF,HALF                                                        
         OC    DELOFL,DELOFL       TABLE OF DELETED CODES                       
         BZ    OFLB52                                                           
         LA    R2,DELOFL                                                        
         LA    R4,L'DELOFL/2                                                    
         MVC   HALF,=H'-1'         KNOCK ONE OFF FOR DELETED CODE               
         B     OFLB54                                                           
OFLB52   OC    ADDOFL,ADDOFL       TABLE OF ADDED CODES                         
         BZ    OFLBLDX                                                          
         LA    R2,ADDOFL                                                        
         LA    R4,L'ADDOFL/2                                                    
         MVC   HALF,=H'1'          INCREMENT FOR ADDED CODE                     
OFLB54   SR    R5,R5                                                            
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
OFLB56   OC    0(L'OFFKOFF,R2),0(R2)                                            
         BZ    OFLB62                                                           
         MVC   OFFKOFF,0(R2)                                                    
         OC    OFFKOFF,SPACES                                                   
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),ACCFIL,OFFRECD,OFFRECD               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,OFFRECD+(ACRECORD-ACKEYD)                                     
         SR    R0,R0                                                            
         USING OFIELD,R1                                                        
OFLB58   CLI   OFIEL,0             TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   OFIEL,OFIELQ        LOCATE OFFICE INFO ELEMENT                   
         BE    *+14                                                             
         IC    R0,OFILN                                                         
         AR    R1,R0                                                            
         B     OFLB58                                                           
         SR    R0,R0                                                            
         ICM   R0,3,OFIINC                                                      
         AH    R0,HALF                                                          
         STCM  R0,3,OFIINC                                                      
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,OFFRECD,OFFRECD                        
         BE    *+6                                                              
         DC    H'0'                                                             
OFLB62   LA    R2,L'OFFKOFF(R2)              NEXT TABLE ENTRY                   
         BCT   R4,OFLB54                                                        
         CLC   HALF,=H'-1'                   IF WE'VE JUST DONE DELETES         
         BO    OFLB52                        GO ROUND AGAIN FOR ADDS            
*                                                                               
OFLBLDX  LA    R2,LOGOLCH          SET CURSOR TO LIST CODE                      
         B     OKXIT                                                            
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD LONG NAME ELEMENT                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING NAMELD,R1                                                        
BLNEL    MVI   NAMEL,NAMELQ                                                     
         LA    R2,LOGOLLNH                                                      
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    ERRMISS                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   NAMEREC(0),LOGOLLN                                               
         AH    RF,=Y(NAMLN1Q+1)                                                 
         STC   RF,NAMLN                                                         
         AR    R1,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* BUILD SHORT NAME ELEMENT                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING SNMELD,R1                                                        
BSNEL    MVI   SNMEL,SNMELQ                                                     
         LA    R2,LOGOLSNH                                                      
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    ERRMISS                                                          
         BCTR  RF,0                                                             
         MVC   SNMNAME,SPACES                                                   
         EX    RF,*+4                                                           
         MVC   SNMNAME(0),LOGOLSN                                               
         MVI   SNMLN,SNMLNQ                                                     
         LA    R1,SNMLNQ(R1)                                                    
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* BUILD OFFICE RECORD INFO ELEMENT                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING OFIELD,R1                                                        
BOIEL    XC    0(OFILNQ,R1),0(R1)                                               
         MVI   OFIEL,OFIELQ                                                     
         MVI   OFILN,OFILNQ                                                     
         TM    BLDFLG,OFFLSTQ                                                   
         BZ    *+8                                                              
         OI    OFISTAT,OFISLIST                                                 
         LA    R1,OFILNQ(R1)                                                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LIST ELEMENT ROUTINE - SORTS OFFICE CODES, CREATES ELEMENT,         *         
* BUILDS A TWO-PART WORK TABLE OF CODES DELETED AND ADDED,            *         
* WEEDS OUT DUPLICATED CODES                                          *         
***********************************************************************         
         SPACE 1                                                                
BOLEL    NTR1                                                                   
         LR    R4,R1               R4=A(LIST ELEMENT)                           
         USING OFLELD,R4                                                        
*        BAS   RE,SCRINIT                                                       
         XC    ELEMENT,ELEMENT     ENSURE AREA IS CLEAR                         
         XC    OFLSEQ,OFLSEQ                                                    
         MVI   OFLEL,OFLELQ        BUILD OFFICE LIST ELEMENT                    
         LA    R3,OFLNTRY                                                       
         LA    R5,SVOFF                                                         
*                                                                               
BOL2     OC    0(2,R5),0(R5)                                                    
         BZ    BOL4                                                             
         MVC   WORK(L'OFLNTRY),0(R5)                                            
         OC    WORK(L'OFLNTRY),SPACES                                           
         CLC   LOGOLC,WORK         ENSURE OFFICE LIST CODE IS NOT               
         BE    BOLERRX             A MEMBER OF ITS OWN LIST (!)                 
         BAS   RE,BOLSCHL          COMPARE WITH TABLE                           
         MVC   0(L'OFLNTRY,R3),0(R5)                                            
         OC    0(L'OFLNTRY,R3),SPACES                                           
         LA    R3,L'OFLNTRY(R3)                                                 
         LA    R5,2(R5)                                                         
         B     BOL2                                                             
*                                                                               
BOL4     SR    R3,R4                                                            
         STC   R3,OFLLN            LENGTH OF ELEMENT                            
BOL6     LA    R3,OFLNTRY          BUBBLE LIST INTO ALPHA SEQUENCE              
         NI    BLDFLG,X'FF'-SWAPPED                                             
BOL8     OC    L'OFLNTRY(L'OFLNTRY,R3),L'OFLNTRY(R3)                            
         BZ    BOL12                         END OF CODES                       
         CLC   0(L'OFLNTRY,R3),L'OFLNTRY(R3) COMPARE WITH NEXT                  
         BE    BOLERR                        DUPLICATE                          
         BL    BOL10                         CORRECT SEQUENCE                   
         XC    0(L'OFLNTRY,R3),L'OFLNTRY(R3) SWAP THEM                          
         XC    L'OFLNTRY(L'OFLNTRY,R3),0(R3)                                    
         XC    0(L'OFLNTRY,R3),L'OFLNTRY(R3)                                    
         OI    BLDFLG,SWAPPED                                                   
BOL10    LA    R3,L'OFLNTRY(R3)              POINT TO NEXT CODE                 
         B     BOL8                                                             
BOL12    TM    BLDFLG,SWAPPED                DID WE DO ANYTHING                 
         BO    BOL6                          YES - GO ROUND AGAIN               
         CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD ROUTINE - ADDS OR WRITES RECORD - HANDLES RECORD      *         
* ACTIVITY POINTERS FOR OFFICE LISTS IF NECESSARY                     *         
* AT ENTRY, P1 BYTE 0 = ADD/PUT BYTES 1-3 = A(RECORD)                 *         
***********************************************************************         
*                                                                               
UPREC    NTR1                                                                   
         MVC   DUB+0(1),0(R1)      SAVE ADD/PUT FLAG                            
         L     R3,0(R1)            GET A(RECORD)                                
         LA    R3,0(R3)            CLEAR HOB                                    
         USING OFFRECD,R3                                                       
         MVI   DUB+1,C'N'          SET RECORD ACTIVITY FLAG                     
         CLI   RAPTR,C'Y'          TEST FOR RECORD ACTIVITY POINTERS            
         BNE   UPREC10             NO                                           
*                                                                               
         TM    OFFRECD+(ACSTATUS-ACKEYD),OFFSLIST                               
         BZ    UPREC10             NOT AN OFFICE LIST                           
         MVI   DUB+1,C'Y'          SET RECORD ACTIVITY FLAG                     
*                                                                               
         LA    R5,BLOCK                                                         
         USING ACRAPD,R5                                                        
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM                                                 
         MVC   RAPCPY,COMPANY                                                   
         MVI   RAPRTYP,RAPKROFL    RECORD=OFFICE LIST                           
         MVI   RAPEMU,C'Y'                                                      
         MVC   RAPACOM,COMFACS                                                  
         ST    R3,RAPAREC                                                       
         GOTO1 RAPPER,RAPBLK                                                    
         BE    UPREC10                                                          
         DC    H'0'                                                             
*                                                                               
UPREC10  LA    R2,DMADD            R2=A(COMMAND)                                
         CLI   DUB+0,UPADD         TEST TO ADD RECORD                           
         BE    *+8                 YES                                          
         LA    R2,DMWRT                                                         
*                                                                               
UPREC12  GOTO1 DATAMGR,DMCB,(R2),ACCFIL,OFFRECD,OFFRECD                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPREC15  CLI   DUB+1,C'Y'                                                       
         BNE   UPRECX                                                           
*                                                                               
         MVI   RAPACTN,RAPAPTR                                                  
         GOTO1 RAPPER,RAPBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPRECX   B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLES OF CODES DELETED AND ADDED BY AMENDMENT SO WE CAN      *         
* UPDATE THEM LATER                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALPF    NTR1                                                                   
         CLI   PFKEY,0                                                          
         BE    VALPFX                                                           
         CLI   PFKEY,PFUP          PAGE UP                                      
         BNE   VALPF10                                                          
         MVI   CURSCR,FSTSCR       RELOAD FIRST SCREEN                          
         MVI   OFFDSP,0            START AT BEGINNING OF OFFICE LIST            
         BAS   RE,DISOFF                                                        
         B     VALPFX                                                           
*                                                                               
VALPF10  CLI   PFKEY,PFDWN         PAGE DOWN                                    
         BNE   ERRPF                                                            
         CLI   OFFNUM,SCRMAX       DO WE HAVE A FULL SCREEN?                    
         BL    ERRPF                                                            
         CLI   LOGACT,C'I'         FOR INQUIRY DON'T PAGE DOWN UNLESS           
         BNE   *+12                                                             
         CLI   OFFNUM,SCRMAX       WE HAVE MORE THAN A FULL SCREEN              
         BE    ERRPF                                                            
         CLI   CURSCR,LSTSCR       ERROR IF ALREADY AT LAST SCREEN              
         BE    ERRLPG                                                           
         MVI   CURSCR,LSTSCR       RELOAD LAST SCREEN                           
         MVI   OFFDSP,SCRMAXQ      POINT TO NEXT SCREEN                         
         BAS   RE,DISOFF                                                        
*                                                                               
VALPFX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SAVE THE CURRENT SCREEN TWA0 IN TWA3                                *         
***********************************************************************         
         SPACE 1                                                                
SVSCR    NTR1                                                                   
         CLI   PFKEY,0                                                          
         BNE   SVSCRX                                                           
*                                                                               
         LA    R3,SVOFF1           POINT TO 1ST SCREEN                          
         CLI   CURSCR,FSTSCR       ON 1ST SCREEN?                               
         BE    *+8                                                              
         LA    R3,SVOFF2           POINT TO SECOND SCREEN                       
         XC    0(SCRMAXQ,R3),0(R3)  CLEAR CURRENT SCREEN AREA                   
*                                                                               
SVSCR10  L     R2,ANXTCLL                                                       
         CLI   8(R2),X'40'         ONLY SIGNIFICANT DATA                        
         BNH   SVSCR20                                                          
         MVC   0(2,R3),8(R2)                                                    
         OC    0(2,R3),SPACES                                                   
         LA    R3,2(R3)            BUMP TO NEXT POS IN OFFICE LIST              
SVSCR20  BAS   RE,NXTENT           BUMP TO NEXT FIELD                           
         BE    SVSCR10                                                          
*                                                                               
         MVC   SVOFFPV,SVOFF       SAVE OFFICE LIST AND UPDATE IT               
         XC    SVOFF,SVOFF                                                      
         MVI   OFFNUM,0                                                         
         LA    R3,SVOFF            OFFICE LIST FROM SCREEN                      
         LA    R4,SVOFFPV          SAVED AREA FOR OFF LIST UPDATE               
         LA    R5,SVOFFMX          MAX ENTRIES IN SVOFF                         
SVSCR30  OC    0(2,R4),0(R4)                                                    
         BZ    SVSCR40                                                          
         MVC   0(2,R3),0(R4)                                                    
         SR    R1,R1                                                            
         IC    R1,OFFNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OFFNUM                                                        
*                                                                               
         LA    R3,2(R3)                                                         
SVSCR40  LA    R4,2(R4)                                                         
         BCT   R5,SVSCR30                                                       
         BAS   RE,SCRINIT          RE-INTIALIZE SCREEN                          
         BAS   RE,DISOFF           RE-DISPLAY OFFICE ONTO SCREEN                
*                                                                               
SVSCRX   BAS   RE,SCRINIT          RE-INTIALIZE SCREEN                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* DISPLAY SCREEN FROM OFFICE LIST                                    *          
**********************************************************************          
         SPACE 1                                                                
DISOFF   NTR1                                                                   
         TWAXC LOGOC0AH,LOGOC0LH,PROT=Y                                         
         CLI   OFFNUM,0            ANYTHING IN OFFICE LIST?                     
         BE    DISOX               NO-EXIT                                      
         LA    R2,LOGOC0AH                                                      
*                                                                               
         USING OFLNTRY,R4                                                       
         LA    R4,SVOFF            USE OFFICE LIST IF ANYTHING IN IT            
         SR    R0,R0                                                            
         IC    R0,OFFDSP           BUMP TO DESIRED POSITION                     
         AR    R4,R0                                                            
         SRA   R0,1                                                             
         SR    R5,R5                                                            
         IC    R5,OFFNUM                                                        
         SR    R5,R0               NUMBER OF ENTRIES IN LIST                    
         BP    DISO10                                                           
         BZ    DISOX                                                            
         DC    H'0'                                                             
*                                                                               
DISO10   MVC   8(L'OFLNTRY,R2),OFLNTRY                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         USING OFFRECD,R3                                                       
         LA    R3,IO2                                                           
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,OFLNTRY                                                  
         OC    OFFKOFF,SPACES                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,OFFRECD,OFFRECD                       
         BNE   ERRNFND             OFFICE REC DOESN'T EXIST                     
*                                                                               
         LA    R1,OFFRECD+(ACRECORD-ACKEYD)                                     
         SR    R0,R0                                                            
         USING SNMELD,R1                                                        
DISO20   CLI   SNMEL,0             LOOK FOR SHORT NAME ELEMENT                  
         BE    DISOX                                                            
         CLI   SNMEL,SNMELQ                                                     
         BE    *+14                                                             
         IC    R0,SNMLN                                                         
         AR    R1,R0                                                            
         B     DISO20                                                           
*                                                                               
         L     R2,ANXTCLR                                                       
         MVC   8(L'SNMNAME,R2),SNMNAME                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,NXTENT                                                        
         BNE   DISOX                                                            
         LA    R4,L'OFLNTRY(R4)    NEXT OFFICE CODE                             
         L     R2,ANXTCLL                                                       
         BCT   R5,DISO10                                                        
*                                                                               
DISOX    LA    R2,LOGOLCH                                                       
         B     OKXIT                                                            
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD TABLES OF CODES DELETED AND ADDED BY AMENDMENT SO WE CAN      *         
* UPDATE THEM LATER                                                   *         
*        R5 = A(CURRENT POS IN SVOFF)                                 *         
***********************************************************************         
         SPACE 1                                                                
BOLSCHL  OC    DELOFL,DELOFL       NOTHING (LEFT) IN DELETES TABLE              
         BZ    BLS3                                                             
         LA    R1,DELOFL                                                        
         LA    R0,L'DELOFL/2                                                    
BLS2     MVC   WORK(L'OFLNTRY),0(R5)                                            
         OC    WORK(L'OFLNTRY),SPACES                                           
         CLC   0(L'OFLNTRY,R1),WORK                                             
         BE    BLS6                                                             
         LA    R1,L'OFLNTRY(R1)                                                 
         BCT   R0,BLS2                                                          
BLS3     LA    R1,ADDOFL           NO MATCH, SO THIS IS A NEW ONE               
         LA    R0,L'ADDOFL/2                                                    
BLS4     OC    0(L'OFLNTRY,R1),0(R1)                                            
         BZ    *+12                                                             
         LA    R1,L'OFLNTRY(R1)                                                 
         BCT   R0,BLS4                                                          
         MVC   0(L'OFLNTRY,R1),0(R5)     SAVE IT IN ADDS TABLES                 
         OC    0(L'OFLNTRY,R1),SPACES                                           
         BR    RE                                                               
BLS6     XC    0(L'OFLNTRY,R1),0(R1)     DELETE IF MATCHED                      
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* FIND DUPLICATE OFFICE CODE ON SCREEN                                *         
***********************************************************************         
         SPACE 1                                                                
BOLERR   BAS   RE,SCRINIT                                                       
         OI    BLDFLG,DUPFND       INIT FLAG                                    
BOLERR10 L     R2,ANXTCLL                                                       
         BAS   RE,NXTENT           SET UP FOR NEXT                              
         BNE   BOLERR20                                                         
         MVC   WORK(L'OFLNTRY),8(R2)                                            
         OC    WORK(L'OFLNTRY),SPACES                                           
         CLC   0(L'OFLNTRY,R3),WORK                                             
         BNE   BOLERR10            MATCHED                                      
         ST    R2,FULL             SAVE ANY ADDRESS FOUND-2 SCREENS             
         XI    BLDFLG,DUPFND                                                    
         TM    BLDFLG,DUPFND                                                    
         BNO   BOLERR10            NOW FIND 2ND OCCURRENCE-IF ANY               
*                                                                               
BOLERR20 L     R2,FULL                                                          
BOLERRX  LTR   RB,RB                                                            
         XIT1  REGS=(R2)           SET THE CURSOR TO IT                         
         EJECT                                                                  
***********************************************************************         
* DELETE AN ELEMENT                                                   *         
***********************************************************************         
         SPACE 1                                                                
HELDEL   LR    R0,RE                                                            
         MVC   DMCB+4(4),DMCB                                                   
         GOTO1 VHELLO,DMCB,(C'D',ACCFIL),,0                                     
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ADD AN ELEMENT                                                      *         
***********************************************************************         
         SPACE 1                                                                
HELADD   LR    R0,RE                                                            
         MVC   DMCB+8(4),DMCB+4                                                 
         MVC   DMCB+4(4),DMCB+0                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCFIL)                                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE SCREEN VARIABLES                                         *         
***********************************************************************         
         SPACE 1                                                                
SCRINIT  LA    R0,NUMCOLSQ                                                      
         LA    R1,NUMLINSQ                                                      
         STM   R0,R1,COORDS                                                     
         LA    R1,LOGOC0AH                                                      
         ST    R1,ANXTLIN                                                       
         ST    R1,ANXTCLL                                                       
         LA    R1,LOGON0AH-LOGOC0AH(R1)                                         
         ST    R1,ANXTCLR                                                       
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* POINT TO NEXT OFFICE LIST ENTRY                                     *         
***********************************************************************         
         SPACE 1                                                                
NXTENT   NTR1                                                                   
         LM    R0,R1,COORDS                                                     
         L     RF,ANXTCLL                                                       
         LA    RF,LOGOC0BH-LOGOC0AH(RF)   NEXT COLUMN                           
         BCT   R0,NXT8                                                          
         L     RF,ANXTLIN                                                       
         LA    RF,LOGOC1AH-LOGOC0AH(RF)   NEXT LINE                             
         LA    R0,NUMCOLSQ         REFRESH COLUMN COUNT                         
         BCT   R1,NXT4                                                          
         B     NXTEOS                                                           
*                                                                               
NXT4     ST    RF,ANXTLIN          SAVE THE ADDRESS(ES)                         
NXT8     ST    RF,ANXTCLL          LH FIELD                                     
         LA    RF,LOGON0AH-LOGOC0AH(RF)                                         
         ST    RF,ANXTCLR          RH FIELD                                     
         B     NXTXIT                                                           
*                                                                               
NXTXIT   STM   R0,R1,COORDS                                                     
         CR    RB,RB               CC EQ = OK                                   
         B     EXIT                                                             
*                                                                               
NXTEOS   LTR   RB,RB               CC NE = END OF SCREEN FOUND                  
         B     EXIT                                                             
         EJECT                                                                  
TALPHA   LA    R0,2                                                             
*                                                                               
TALPH2   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERROFF                                                           
         B     TALPH4                                                           
         CLI   0(R1),C'A'                                                       
         BL    ERROFF                                                           
         CLI   0(R1),C'I'                                                       
         BNH   TALPH4                                                           
         CLI   0(R1),C'J'                                                       
         BL    ERROFF                                                           
         CLI   0(R1),C'R'                                                       
         BNH   TALPH4                                                           
         CLI   0(R1),C'S'                                                       
         BL    ERROFF                                                           
         CLI   0(R1),C'Z'                                                       
         BH    ERROFF                                                           
*                                                                               
TALPH4   LA    R1,1(R1)                                                         
         BCT   R0,TALPH2                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRLPG   LA    RF,AE$LSTPG         ALREADY AT LAST PAGE                         
         LA    R2,LOGACTH                                                       
         B     INVERR                                                           
ERRPF    MVI   ERROR,AE$ACX2P      PF KEY INVALID FOR THIS SCREEN               
         B     CSXIT                                                            
ERRMISS  MVI   ERROR,AC$MISIF      MISSING INPUT FIELD                          
         B     CSXIT                                                            
ERRDSK   MVI   ERROR,AC$DISCE      NON-RECOVERABLE DISK ERROR                   
         B     CSXIT                                                            
ERRDUP   MVI   ERROR,AC$DUPIF      DUPLICATE LIST CODE                          
         B     CSXIT                                                            
ERRFND   MVI   ERROR,AC$REXIS      RECORD ALREADY EXISTS                        
         B     CSXIT                                                            
ERRCDEL  MVI   ERROR,AC$OLDEL      CODES LIST USED - CAN'T DELETE               
         B     CSXIT                                                            
ERRNFND  MVI   ERROR,AC$RECNF      RECORD NOT FOUND                             
         B     CSXIT                                                            
ERRLST   LA    RF,AE$NOLST         CODE IS AN OFFICE LIST                       
         B     INVERR                                                           
ERRMEM   LA    RF,AE$OLCOL         CODE IS A MEMBER OF AN OFFICE LIST           
         B     INVERR                                                           
ERRADD   MVI   ERROR,AC$RTBIG      RECORD TOO BIG                               
         B     CSXIT                                                            
ERRSHORT MVI   ERROR,AC$IFSHR      INPUT FIELD TOO SHORT                        
         B     CSXIT                                                            
ERROFF   LA    RF,AE$INOFF         INVALID OFFICE/OFFICE LIST                   
         B     INVERR                                                           
*                                                                               
CLRXIT   SR    R1,R1               CLEAR TO END OF SCREEN                       
         IC    R1,0(R2)                                                         
         LA    R1,0(R1,R2)                                                      
         TWAXC (R1)                                                             
         B     CSXIT                                                            
         EJECT                                                                  
*********************************************************************           
* ERROR ROUTINE                                                     *           
*********************************************************************           
         SPACE 1                                                                
INVERR   XC    WORK,WORK                                                        
         GOTO1 VGETTXT,WORK,(RF),(0,LOGHEADH),(C'E',0)                          
         MVI   ERROR,X'FE'                                                      
         B     CSXIT                                                            
         EJECT                                                                  
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CONSTANTS                                                         *           
*********************************************************************           
         SPACE 1                                                                
DMREAD   DC    C'DMREAD '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
ACCFIL   DC    C'ACCOUNT'                                                       
*                                                                               
* PFKEY EQUATES                                                                 
*                                                                               
SCRMAX   EQU   60                  MAX ENTRIES PER SCREEN                       
SCRMAXQ  EQU   120                 RELATIVE POSITIONS INTO OFFLIST              
FSTSCR   EQU   1                   FIRST SCREEN                                 
LSTSCR   EQU   2                   LAST SCREEN                                  
*                                                                               
* PFKEY EQUATES                                                                 
*                                                                               
PFUP     EQU   7                   PAGE UP                                      
PFDWN    EQU   8                   PAGE DOWN                                    
         EJECT                                                                  
*********************************************************************           
* ++INCLUDES                                                        *           
*********************************************************************           
         SPACE 1                                                                
       ++INCLUDE ACLFMWORK                                                      
         SPACE 1                                                                
* PROGRAM WORKING STORAGE COVERED BY RC                                         
         SPACE 1                                                                
COORDS   DS    2F                                                               
RETA     DS    F                                                                
VHELLO   DS    V                                                                
VGETTXT  DS    V                                                                
ANXTLIN  DS    A                                                                
ANXTCLL  DS    A                                                                
ANXTCLR  DS    A                                                                
SAVSTAT  DS    XL1                                                              
BLDFLG   DS    X                   GP FLAG USED IN BUILD ROUTINE                
OFFLSTQ  EQU   X'80'               OFFICE RECORD IS A LIST                      
DUPFND   EQU   X'08'               DUPLICATE CODE FOUND IN OFFICE LIST          
SWAPPED  EQU   X'01'               OFFICE LIST WAS RESEQUENCED                  
*                                                                               
OFISV    DS    XL(OFILNQ)          SAVE AREA FOR OFI ELEMENT                    
DELOFL   DS    XL(L'SVOFF)                                                      
ADDOFL   DS    XL(L'SVOFF)                                                      
*                                                                               
NUMCOLSQ EQU   4                                                                
NUMLINSQ EQU   15                                                               
UPADD    EQU   C'A'                                                             
UPPUT    EQU   C'P'                                                             
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMD8D                                                       
         EJECT                                                                  
********************************************************************            
* LOCAL STORAGE                                                    *            
********************************************************************            
         SPACE 1                                                                
CURSCR   DS    CL1                 CURRENT SCREEN                               
LSTACT   DS    CL1                 SAVED AREA FOR LAST ACTION                   
LSTOFF   DS    CL2                 SAVED AREA FOR LAST OFFICE CODE              
OFFDSP   DS    XL1                 DISP INTO OFFICE LIST                        
OFFNUM   DS    XL1                 NUMBER OF ENTRIES IN OFF LIST                
*                                                                               
SVOFF    DS    0CL240              SAVED OFFICE                                 
SVOFF1   DS    CL120                                                            
SVOFF2   DS    CL120                                                            
SVOFFMX  EQU   (*-SVOFF)/2         MAX ENTRIES IN SVOFF                         
*                                                                               
SVOFFPV  DS    CL240               SAVED OFFICE                                 
         EJECT                                                                  
         ORG   T603FFD                                                          
*                                                                               
* INCLUDE FATWA                                                                 
*                                                                               
*                                                                               
TWAMXLEN EQU   6144                MAXIMUM LENGTH OF OLD TWA RECORD             
TWAMAXRL EQU   14336               MAXIMUM LENGTH OF NEW TWA RECORD             
TWAMAX   EQU   18432               MAXIMUM LENGTH OF LATEST  RECORD             
*                                                                               
TWATASK  DS    C         +0        PROCESSING TASK NUMBER                       
TWAOFFC  DS    C         +1        OFFICE CODE                                  
TWATRM   DS    0H        +2        TERMINAL NUMBER                              
TWAFLDNM DS    X         +2        DDREQTWA - SAVE NUMBER OF LAST FIELD         
         DS    X         +3        N/D OFFLINE                                  
TWASAGN  DS    XL2       +4        ACCESS GROUP# (IF PGMIND2=PGMISECA)          
TWAACCS  DS    CL4       +6        LIMIT ACCESS CODE                            
TWAUSRID DS    XL2       +10       CONNECT ID NUM (OLAI)                        
TWAAUTH  DS    XL2       +12       AUTHORIZATION CODE                           
TWAAGY   DS    CL2       +14       EBCDIC AGENCY CODE                           
*                                                                               
TWALEN   EQU   TWASAGN,2           MESSAGE LENGTH                               
TWASMI   EQU   TWAUSRID,2          DISPLACEMENT TO SMI (NON-OLAI)               
*                                                                               
TWAUSER  DS    CL48      +16       AVAILABLE FOR USER                           
         EJECT                                                                  
* ACLFMEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACERREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACERREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACRAPPERD (ACRAPD)                                                            
         PRINT OFF                                                              
ACRAPD   DSECT                                                                  
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083ACLFM2B   05/01/02'                                      
         END                                                                    
