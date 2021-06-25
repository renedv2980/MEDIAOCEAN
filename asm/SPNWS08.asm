*          DATA SET SPNWS08    AT LEVEL 091 AS OF 02/26/07                      
*PHASE T20708C,*                                                                
         TITLE 'BWS08 - BUYERS WORKSHEET PACKAGE/ORBIT OVERLAY'                 
T20708   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20708**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         MVC   APPARM,COMPARM      RESTORE APPARM                               
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         A     R1,APRELO                                                        
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         LA    RE,NPKGLINS         SET N'LINES PER SCREEN                       
         CLI   INREC,RECPKG                                                     
         BE    *+8                                                              
         LA    RE,NORBLINS                                                      
         STC   RE,LNLINES                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         DC    AL4(0)  VALKEY                                                   
         DC    AL4(0)  VALREC                                                   
         DC    AL4(0)  DISKEY                                                   
         DC    AL4(0)  DISREC                                                   
         DC    AL4(0)  DELREC                                                   
         DC    AL4(0)  RESREC                                                   
         B     VALPAR                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     VALSEL                                                           
         B     FSTLST                                                           
         DC    AL4(0)  PROCLS                                                   
         B     FSTSCR                                                           
         B     LSTSCR                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     PUTKEY                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* FIRST TIME HOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   MVI   SVACTIV2,0          INDICATE NO ACTIVITY YET                     
         XC    LSVGLPTS(NMAXWKS*4),LSVGLPTS                                     
         CLI   SCSELLVL,1          TEST PACKAGE SCREEN WAS SELECTED             
         BNH   FLSTX               FROM HIGHER LEVEL LIST SCREEN                
         ZIC   RE,BWDKELPO         YES - EXTRACT PACKAGE NUMBER                 
         CVD   RE,APDUB                  AND DISPLAY IT                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PKGNUM,APDUB                                                     
         OI    PKGNUMH+6,FVOXMT                                                 
         LA    R1,PKGPFKH                                                       
         CLI   INREC,RECPKG                                                     
         BE    *+8                                                              
         LA    R1,ORBPFKH                                                       
         XC    8(L'PKGPFK,R1),8(R1)      DISPLAY PF KEY HELP LINE               
         MVC   8(L'PFKLINE,R1),PFKLINE                                          
         OI    6(R1),FVOXMT                                                     
*                                                                               
FLSTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVFOK IF KEY IS INVALID                         *         
*          APRECKEY                                                   *         
*          APPARM FOR ROOT                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   DS    0H                                                               
         GOTO1 AVALPARM,BWSKY2H    VALIDATE SELECT PARAMETERS                   
         BE    *+16                                                             
         TM    TWAFLAG,TWANODET                                                 
         BO    ENWR                                                             
         B     VALPX                                                            
*                                                                               
         CLI   CMPNWKS,14          ARE WE MORE THAN 14 UNITS?                   
         BNH   VALP2               NO, WE SHOULD OKAY                           
         TM    CMPOPTS,CAMODLY     ARE WE A DAILY CAMPAIGN?                     
         BO    E53DYS                                                           
         B     E53WKS                                                           
*                                                                               
VALP2    MVI   LFLAG,0                                                          
         XC    LADEMEL,LADEMEL                                                  
         XC    LACMTEL,LACMTEL                                                  
         CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BNE   VALP10                                                           
         GOTO1 AFVAL,PKGNUMH       YES-VALIDATE PACKAGE NUMBER                  
         BH    VALPX                                                            
         BE    VALP10              FOUND-ASSUME RECORD ALREADY ADDED            
         TM    CMPOPTS,CAMODLY     TEST DAILY SCHEDULE                          
         BZ    VALP4                                                            
         CLI   INREC,RECORB        AND ORBIT                                    
         BE    EDLYORB             YES-ERROR                                    
*                                                                               
VALP4    OI    LFLAG,LNEWPKG       INDICATE NEW PACKAGE                         
         CLI   BDPT,0              TEST DAYPART/LENGTH SPECIFIED                
         BE    VALP97                                                           
         CLI   BSLN,0                                                           
         BE    VALP98                                                           
         MVC   IOKEY,APRECKEY                                                   
         LA    R3,IOKEY            FIND FIRST AVAILABLE PACKAGE SEQ NUM         
         MVI   BWDKELPO,1                                                       
         SR    R4,R4                                                            
*                                                                               
VALP6    GOTO1 AMIN,MINHI1                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALP8                                                            
         CLC   IOKEY(BWDKELPO-BWDKEY),IOKEYSAV                                  
         BNE   VALP8                                                            
         IC    R4,BWDKELPO                                                      
         MVC   BWDKELDY(4),XFF     SKIP TO NEXT PACKAGE                         
         B     VALP6                                                            
*                                                                               
VALP8    LA    R4,1(R4)            R4=FIRST AVAILABLE NUM                       
         STC   R4,LPKGNUM                                                       
         CVD   R4,APDUB            DISPLAY NEW PACKAGE NUMBER                   
         OI    APDUB+7,X'0F'                                                    
         UNPK  PKGNUM,APDUB                                                     
         OI    PKGNUMH+6,FVOXMT                                                 
*                                                                               
         L     R3,AIOAREA1                                                      
         XC    BWDRECD(256),BWDRECD  BUILD PACKAGE MASTER RECORD                
         MVC   BWDKEY,APRECKEY                                                  
         MVC   BWDKELPO,LPKGNUM                                                 
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         MVI   BWDKELSQ,0                                                       
         LA    R0,BWDEL+BWDELLNQ+1-BWDRECD                                      
         STCM  R0,3,BWDLEN                                                      
         MVI   BWDELCD,BWDELCDQ                                                 
         MVI   BWDELLN,BWDELLNQ                                                 
         MVC   BWDSTACD,BWDKELST                                                
         MVC   BWDPKOR,LPKGNUM                                                  
         MVC   BWDSTA,QSTA         STATION                                      
         MVC   BWDDPT,BDPT                                                      
         MVC   BWDSLN,BSLN                                                      
         MVI   BWDINDS,BWDIPKG     INDICATE PACKAGE                             
         CLI   INREC,RECORB        TEST ORBIT                                   
         BNE   *+8                                                              
         MVI   BWDINDS,BWDIORB     YES-INDICATE ORBIT                           
         MVC   BWDPROG,SPACES                                                   
         B     VALP18                                                           
*                                                                               
VALP10   MVI   FVMINL,1            ACTION IS NOT ADD --                         
         GOTO1 AFVAL,PKGNUMH       VALIDATE PACKAGE NUMBER                      
         BNE   VALPX                                                            
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BZ    VALP99                                                           
         L     R4,SCFULL                                                        
         CH    R4,=H'1'                                                         
         BL    ENOTV                                                            
         CH    R4,=H'255'                                                       
         BH    ENOTV                                                            
         STC   R4,LPKGNUM          SAVE PACKAGE NUMBER                          
         LA    R3,IOKEY                                                         
         MVC   IOKEY,APRECKEY                                                   
         STC   R4,BWDKELPO                                                      
         GOTO1 AMIN,MINHI1         READ HIGH FOR PACKAGE RECORD                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALP12                                                           
         CLC   IOKEY(BWDKELDY-BWDKEY),IOKEYSAV                                  
         BNE   VALP12                                                           
         CLI   BWDKELSQ,0          PACKAGE RECORD-TEST PACKAGE MASTER           
         BE    VALP14              YES                                          
*                                  PACKAGE NOT FOUND -                          
VALP12   CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BNE   EPNF                NO-ERROR                                     
         B     VALP4               YES-CONTINUE ADDING NEW PACKAGE              
*                                                                               
VALP14   L     R3,AIOAREA1         PACKAGE MASTER FOUND -                       
         CLI   INREC,RECORB        CHECK HAVEN'T GOT ORBITS AND                 
         BNE   *+12                PACKAGES MIXED UP                            
         TM    BWDINDS,BWDIORB                                                  
         BZ    EPNF                                                             
         CLI   INREC,RECPKG                                                     
         BNE   *+12                                                             
         TM    BWDINDS,BWDIORB                                                  
         BO    EPNF                                                             
         MVC   BDPT,BWDDPT         SAVE DAYPART AND LENGTH                      
         MVC   BSLN,BWDSLN                                                      
         SR    R0,R0               LOCATE THE ELEMENTS                          
         LA    R4,BWDEL                                                         
*                                                                               
VALP16   CLI   0(R4),0                                                          
         BE    VALP18                                                           
         CLI   0(R4),DMOELCDQ                                                   
         BNE   *+8                                                              
         ST    R4,LADEMEL                                                       
         CLI   0(R4),COMELCDQ                                                   
         BNE   *+8                                                              
         ST    R4,LACMTEL                                                       
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALP16                                                           
*                                                                               
*                                  FIELD VALIDATION --                          
VALP18   GOTO1 AFVAL,PKGDESH       ** DESCRIPTION **                            
         BH    VALPX                                                            
         BE    *+12                                                             
         TM    LFLAG,LNEWPKG       MISSING-TEST NEW PACKAGE                     
         BO    ENONE                       YES-ERROR                            
         TM    LFLAG,LNEWPKG       FOUND-TEST NEW PACKAGE                       
         BO    *+12                                                             
         TM    FVIIND,FVITHIS      NO-TEST FIELD INPUT THIS TIME                
         BZ    VALP20                                                           
         CLC   PKGDES,BWDPROG      YES-TEST CHANGE                              
         BE    VALP20                                                           
         OI    LFLAG,LCHGREC       YES                                          
         MVC   BWDPROG(L'PKGDES),PKGDES                                         
         OI    BWDINDS,BWDIPRG     INDICATE PROGRAM OVERRIDE                    
*                                                                               
VALP20   MVC   PKGDES,BWDPROG                                                   
         OI    PKGDESH+6,FVOXMT                                                 
*                                                                               
         GOTO1 AFVAL,PKGCSTH       ** COST **                                   
         BH    VALPX                                                            
         BE    *+12                                                             
         TM    LFLAG,LNEWPKG       MISSING-TEST NEW PACKAGE                     
         BO    ENONE                       YES-ERROR                            
         TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BO    *+12                                                             
         TM    FVIIND,FVITHIS      NO-TEST FIELD INPUT THIS TIME                
         BZ    VALP22                                                           
         ZIC   RE,FVILEN           YES                                          
         ST    RE,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   APPARM,0                                                         
         BNE   EICS                                                             
         GOTO1 ANETCOST,APPARM+4   NET DOWN THE COST IF NECESSARY               
         CLC   BWDCOST1,APPARM+4   TEST CHANGE OF COST                          
         BE    VALP22                                                           
         OI    LFLAG,LCHGCST       YES                                          
         MVC   BWDCOST1,APPARM+4                                                
*                                                                               
VALP22   XC    PKGCST,PKGCST       EDIT THE COST                                
         OI    PKGCSTH+6,FVOXMT                                                 
         OC    BWDCOST1,BWDCOST1                                                
         BNZ   *+14                                                             
         MVC   PKGCST(2),=C'$0'                                                 
         B     VALP24                                                           
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
         LA    RE,PKGCST                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'PKGCST                                                  
         LA    RE,BWDCOST1                                                      
         ST    RE,EBAIN                                                         
         CLC   BWDCOST1,=F'10000'                                               
         BL    *+12                                                             
         MVI   EBSCIN,X'82'                                                     
         MVI   EBDECS,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
VALP24   GOTO1 AFVAL,PKGDATH       ** DATES **                                  
         BH    VALPX                                                            
         BE    *+16                                                             
         TM    LFLAG,LNEWPKG       MISSING-TEST NEW PACKAGE                     
         BO    VALP26              YES                                          
         B     VALP25              NO-DISPLAY THE DATES                         
         TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BO    *+12                                                             
         TM    FVIIND,FVITHIS      NO-TEST FIELD INPUT THIS TIME                
         BZ    VALP25                                                           
         GOTO1 AVALDATE                                                         
         BNE   VALPX                                                            
         CLC   APFULL,BWDDATES     TEST DATES HAVE CHANGED                      
         BE    VALP25                                                           
         OI    LFLAG,LCHGDAT       YES                                          
         MVC   BWDDATES,APFULL                                                  
         MVC   BWDWKS,APHALF                                                    
*                                                                               
VALP25   XC    PKGDAT,PKGDAT       DISPLAY DATES                                
         OI    PKGDATH+6,FVOXMT                                                 
         OC    BWDDATES,BWDDATES   TEST ANY DATES                               
         BZ    VALP26                                                           
         OI    LFLAG,LDATES        YES                                          
         GOTO1 VDATCON,APPARM,(2,BWDDATES),(8,PKGDAT)                           
         MVI   PKGDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,BWDDATES+2),(8,PKGDAT+9)                            
*                                                                               
VALP26   GOTO1 AFVAL,PKGDEMH       ** DEMOS **                                  
         BH    VALPX                                                            
         BE    *+12                                                             
         TM    LFLAG,LNEWPKG       MISSING-TEST NEW PACKAGE                     
         BO    ENONE                       YES-ERROR                            
         TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BO    *+12                                                             
         TM    FVIIND,FVITHIS      NO-TEST DEMOS INPUT THIS TIME                
         BZ    VALP54                                                           
         CLI   FVILEN,20           TEST FIELD HAS MORE THAN 20 CHARS            
         BNH   VALP27                                                           
         XC    APELEM,APELEM       YES-SEARCH FOR COMMA                         
         LA    RE,C','                                                          
         STC   RE,APELEM(RE)                                                    
         TRT   FVIFLD,APELEM                                                    
         BZ    VALP28              NO COMMA - MUST BE IN SHORTHAND FMT          
*                                                                               
VALP27   L     R2,AIOAREA3         ALLOW 4 DEMOS                                
         GOTO1 VSCANNER,APPARM,FVIHDR,(4,(R2)),C',=,='                          
         ZIC   R0,4(R1)                                                         
         MVI   APFLAG,0                                                         
         CLI   1(R2),0             TEST DEMOS IN SHORTHAND FORMAT               
         BNE   VALP30                                                           
         CLI   12(R2),C'0'                                                      
         BL    VALP30                                                           
         CLI   4(R1),1                                                          
         BH    ENOTV                                                            
*                                                                               
VALP28   LA    R1,APPARM           YES --                                       
         MVC   4(4,R1),AIOAREA3                                                 
         MVI   4(R1),4                                                          
         CLI   INREC,RECORB                                                     
         BNE   *+8                                                              
         MVI   4(R1),14            ALLOW 14 DEMOS FOR ORBITS                    
         GOTO1 VSCANNER,(R1),FVIHDR,,C',=/='                                    
         IC    R0,4(R1)            R0 = NUMBER OF DEMO VALUES                   
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)                                                       
         BZ    ENOTV                                                            
         BCTR  RE,0                                                             
         MH    RE,=H'3'            TEST TOO MANY                                
         LA    RE,ESTDEMS(RE)                                                   
         OC    0(3,RE),0(RE)                                                    
         BZ    ENOTV               YES                                          
         MVC   APWORK(L'ESTDEMS),ESTDEMS                                        
         MVI   APFLAG,1            APFLAG = 1 FOR SHORTHAND FORMAT              
         B     VALP34                                                           
*                                                                               
VALP30   LTR   R0,R0               TEST SCANNER WAS SUCCESSFUL                  
         BZ    ENOTV                                                            
         XC    FVIFLD,FVIFLD       DEMOS IN EXPANDED FORMAT --                  
         LA    R4,FVIFLD           BUILD FIELD FOR DEMOVAL                      
         LA    R8,1                                                             
*                                                                               
VALP32   STC   R8,FVINDX                                                        
         MVI   FVSUBX,1                                                         
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         BZ    ENONE                                                            
         MVI   FVSUBX,2                                                         
         CLI   1(R2),0                                                          
         BE    ENONE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R2)                                                   
         LA    R4,1(RE,R4)                                                      
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         LA    R2,32(R2)                                                        
         LA    R8,1(R8)                                                         
         BCT   R0,VALP32                                                        
*                                                                               
         XC    FVIHDR,FVIHDR                                                    
         LA    RE,FVIHDR+1                                                      
         SR    R4,RE                                                            
         STC   R4,FVTLEN                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
*                                                                               
         LA    RE,ESTUSRNM                                                      
         ST    RE,APPARM+12        VALIDATE DEMO NAMES                          
*                                                                               
         LA    RE,ESTDEMS                                                       
         SR    RF,RF                                                            
VALP33   OC    0(3,RE),0(RE)                                                    
         BZ    VALP33X                                                          
         CLI   1(RE),X'21'         USER DEFINED?                                
         BNE   *+8                                                              
         LA    RF,1(RF)            BUMP UP NUMBER OF USER DEFINED               
*                                                                               
         LA    RE,3(RE)                                                         
         B     VALP33                                                           
VALP33X  STC   RF,APPARM+12                                                     
         GOTO1 VDEMOVAL,APPARM,(1,FVIHDR),(4,APWORK),(C'S',DBLOCK)              
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+14                                                             
         MVC   FVINDX,0(R1)                                                     
         B     ENOTV                                                            
*                                                                               
VALP34   TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BZ    VALP36                                                           
         XC    APELEM,APELEM       YES - BUILD DEMO ELEMENT                     
         LA    R8,APELEM                                                        
         USING DMOEL,R8                                                         
         MVI   DMOELCD,DMOELCDQ                                                 
         LA    R9,DMODEMO                                                       
*                                                                               
VALP36   L     R2,AIOAREA3                                                      
         LA    R4,APWORK                                                        
         XC    LDEMS,LDEMS                                                      
         MVI   FVINDX,1            FOR ALL DEMOS IS LIST --                     
*                                                                               
VALP38   LA    RE,ESTDEMS          FIND DEMO IN EST LIST                        
         CLI   APFLAG,1                                                         
         BE    VALP40                                                           
         MVI   FVSUBX,1                                                         
*                                                                               
VALP40   OC    0(3,RE),0(RE)                                                    
         BZ    EDNF                                                             
         CLC   1(2,R4),1(RE)                                                    
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     VALP40                                                           
         LA    RE,LDEMS            FOUND - TEST FOR DUPLICATE                   
         LA    RF,4                                                             
*                                                                               
VALP42   OC    0(3,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(3,RE),0(R4)                                                    
         B     VALP44                                                           
         CLC   0(3,RE),0(R4)                                                    
         BE    EDUP                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,VALP42                                                        
*                                                                               
VALP44   TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BZ    *+14                                                             
         MVC   0(3,R9),0(R4)       YES - MOVE DEMO INTO ELEMENT                 
         B     VALP46                                                           
         ICM   R8,15,LADEMEL       NO - FIND DEMO IN ELEMENT                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DMOEL,R8                                                         
         ZIC   RF,DMOELLN                                                       
         AR    RF,R8                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
         LA    R9,DMODEMO                                                       
         CLC   1(2,R9),1(R4)                                                    
         BE    VALP46                                                           
         BXLE  R9,RE,*-10                                                       
         XC    APELEM,APELEM       DEMO NOT FOUND IN ELEMENT --                 
         ZIC   RF,1(R8)            ADD THE DEMO TO THE ELEMENT                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R8)                                                  
         GOTO1 ADELELS,BWDRECD                                                  
         ZIC   RF,APELEM+1                                                      
         LA    RF,L'DMODEMO(RF)                                                 
         STC   RF,APELEM+1                                                      
         GOTO1 AADDELS,BWDRECD                                                  
         MVC   0(3,R9),0(R4)                                                    
*                                                                               
VALP46   SR    RF,RF               VALIDATE THE DEMO VALUE                      
         CLI   APFLAG,1                                                         
         BE    VALP48                                                           
         MVI   FVSUBX,2                                                         
         ICM   RF,1,1(R2)                                                       
         BZ    ENONE                                                            
         LA    RE,22(R2)                                                        
         B     VALP50                                                           
*                                                                               
VALP48   CLI   1(R2),0                                                          
         BNE   ENOTV                                                            
         IC    RF,0(R2)                                                         
         LA    RE,12(R2)                                                        
*                                                                               
VALP50   ST    RE,APPARM                                                        
         MVI   APPARM,1                                                         
***  2 DECIMAL                                                                  
         MVI   APBYTE,0            APBYTE IS USED NEAR VALP78!!                 
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMAL?                          
         BZ    VALP50G              - NOPE                                      
         CLI   12(R2),C'0'         IS IT A NUMBER?                              
         BL    VALP50C              - NOPE, NORMAL, NOT THE SHORTHAND           
         CLI   1(R4),C'R'          IS IT A RATING?                              
         BE    VALP50E              - YUP                                       
         CLI   1(R4),C'E'          IS IT AN E-RATING?                           
         BE    VALP50E              - YUP                                       
         B     VALP50G             GET OUTTA HERE                               
*                                                                               
VALP50C  CLI   12(R2),C'R'         IS IT A RATING?                              
         BE    VALP50E              - YUP                                       
         CLI   12(R2),C'E'         IS IT AN E-RATING?                           
         BNE   VALP50G              - NOPE                                      
VALP50E  MVI   APPARM,2             - YUP, WE ARE                               
         OI    APBYTE,X'40'        TEMPFLG TO SIGNIFY WE NEED 2 DECIMAL         
***  2 DECIMAL                                                                  
VALP50G  ST    RF,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM                                                  
         CLI   0(R1),FF                                                         
         BE    ENOTV                                                            
*****    OC    APPARM+4(4),APPARM+4   0.0 SHOULD BE ALLOWED IF THEY CAN         
*****    BZ    ENOTV                  CHANGE IT TO 0 LATER                      
         MVC   APFULL,4(R9)                                                     
         NI    APFULL,FF-DMODEMOV                                               
         NI    APFULL,FF-DMODEM2D   TURN OFF 2 DECIMAL                          
         CLC   APFULL,APPARM+4     TEST CHANGE IN DEMO VALUE                    
         BE    VALP52                                                           
         OI    LFLAG,LCHGREC       YES                                          
         MVC   4(4,R9),APPARM+4                                                 
         OI    4(R9),DMODEMOV                                                   
***  2 DECIMAL                                                                  
**       TM    APROFBTS,A00TWODC   WE DOING 2 DECIMAL?                          
**       BZ    VALP52               - NOPE                                      
         TM    APBYTE,X'40'        DID WE DO 2 DECIMAL?                         
         BZ    VALP52               - NOPE                                      
         OI    4(R9),DMODEM2D       - YUP, WE ARE                               
***  2 DECIMAL                                                                  
*                                                                               
VALP52   TM    LFLAG,LNEWPKG       NEXT DEMO                                    
         BZ    *+8                                                              
         LA    R9,L'DMODEMO(R9)                                                 
         LA    R2,32(R2)                                                        
         LA    R4,3(R4)                                                         
         ZIC   RE,FVINDX                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FVINDX                                                        
         BCT   R0,VALP38                                                        
*                                                                               
         TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BZ    VALP54                                                           
         SR    R9,R8               YES - ADD DEMO ELEMENT                       
         STC   R9,DMOELLN                                                       
         GOTO1 AADDELS,BWDRECD                                                  
         ST    R8,LADEMEL                                                       
         DROP  R8                                                               
*                                                                               
VALP54   XC    PKGDEM,PKGDEM       EDIT THE DEMOS                               
         ICM   R4,15,LADEMEL                                                    
         BZ    VALP62                                                           
         USING DMOEL,R4                                                         
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    RE,APFULL                                                        
         ST    RE,EBAIN                                                         
         MVI   EBLOUT,5                                                         
         MVI   EBALIGN,C'L'                                                     
         ZIC   RF,DMOELLN                                                       
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         LA    R0,4                MAX 4 DEMOS                                  
         LA    R2,COMDNAMS                                                      
         LA    R8,ESTDEMS                                                       
         LA    R9,PKGDEM                                                        
         XC    LPRIMRTG,LPRIMRTG                                                
         OI    LFLAG,LFIRST                                                     
*                                                                               
VALP56   OC    0(3,R8),0(R8)                                                    
         BZ    VALP60                                                           
         LA    R1,DMODEMO                                                       
         LA    RE,L'DMODEMO                                                     
         CLC   1(2,R1),1(R8)                                                    
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     VALP58                                                           
         TM    LFLAG,LFIRST                                                     
         BZ    *+14                                                             
         MVC   LPRIMRTG,4(R1)      SAVE PRIMARY DEMO VALUE                      
         NI    LPRIMRTG,FF-DMODEMOV                                             
         MVC   0(6,R9),0(R2)                                                    
         LA    R9,6(R9)                                                         
         CLI   5(R2),C' '                                                       
         BH    *+6                                                              
         BCTR  R9,0                                                             
         MVI   0(R9),C'='                                                       
         LA    R9,1(R9)                                                         
         BRAS  RE,VPEDITDM                                                      
         MVI   0(R9),C','                                                       
         LA    R9,1(R9)                                                         
         BCT   R0,VALP58                                                        
         B     VALP60                                                           
*                                                                               
VALP58   LA    R8,3(R8)            NEXT ESTIMATE DEMO                           
         LA    R2,6(R2)                                                         
         NI    LFLAG,FF-LFIRST                                                  
         B     VALP56                                                           
*                                                                               
VALP60   BCTR  R9,0                                                             
         CLI   0(R9),C','                                                       
         BNE   VALP62                                                           
         MVI   0(R9),C' '                                                       
*                                                                               
VALP62   OI    PKGDEMH+6,FVOXMT                                                 
         OI    PKGDEMH+FVIIND-FVIHDR,FVIVAL                                     
         DROP  R4                                                               
*                                                                               
VALP64   GOTO1 AFVAL,PKGCOMH       ** COMMENT **                                
         BH    VALPX                                                            
         BE    VALP66                                                           
         TM    FVIIND,FVITHIS      MISSING-TEST INPUT THIS TIME                 
         BZ    VALP68                                                           
         MVI   APELEM,COMELCDQ     YES-DELETE COMMENT ELEMENT                   
         GOTO1 ADELELS,BWDRECD                                                  
         OI    LFLAG,LCHGREC                                                    
         B     VALP70                                                           
*                                                                               
VALP66   TM    LFLAG,LNEWPKG       COMMENT - TEST NEW PACKAGE                   
         BO    *+12                                                             
         TM    FVIIND,FVITHIS      NO - TEST INPUT THIS TIME                    
         BZ    VALP68                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,COMELCDQ     YES - DELETE OLD ELEMENT                     
         GOTO1 ADELELS,BWDRECD                                                  
         LA    R4,APELEM           BUILD NEW COMMENT ELEMENT                    
         USING COMEL,R4                                                         
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   COMCOM(0),FVIFLD                                                 
         MVI   COMNUM,1                                                         
         LA    RE,COMCOM-COMEL+1(RE)                                            
         STC   RE,COMELLN                                                       
         GOTO1 AADDELS,BWDRECD     ADD COMMENT ELEMENT                          
         ST    R4,LACMTEL                                                       
         OI    LFLAG,LCHGREC                                                    
*                                                                               
VALP68   XC    PKGCOM,PKGCOM       DISPLAY THE COMMENT                          
         OI    PKGCOMH+6,FVOXMT                                                 
         ICM   R4,15,LACMTEL       THIS CANNOT BE TRUSTED BECAUSE IF            
         BZ    VALP70                A DEMO GETS ADDED, ADDRESS SHIFTS          
         LA    R4,BWDEL            FIND THE ADDRESS OF COMMENT AGAIN            
         XR    RE,RE                                                            
VALP68C  CLI   0(R4),0                                                          
         BE    VALP70                                                           
         CLI   0(R4),COMELCDQ                                                   
         BE    VALP69                                                           
         IC    RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     VALP68C                                                          
*                                                                               
         USING COMEL,R4                                                         
VALP69   ZIC   RE,COMELLN                                                       
         SH    RE,=Y(COMCOM-COMEL)                                              
         LA    R1,COMCOM                                                        
         CLI   COMNUM,1            TEST OLD STYLE COMMENT ELEMENT               
         BL    *+12                                                             
         CLI   COMNUM,5                                                         
         BNH   *+10                                                             
         LA    RE,1(RE)                                                         
         BCTR  R1,0                                                             
         LA    RF,L'PKGCOM                                                      
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PKGCOM(0),0(R1)                                                  
         DROP  R4                                                               
*                                                                               
VALP70   TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BO    VALP74                                                           
         TM    LFLAG,LCHG          NO-TEST ANY CHANGES TO RECORD                
         BZ    VALP74                                                           
         GOTO1 AMIN,MINWRT1        YES-WRITE RECORD BACK                        
         BE    VALP74                                                           
         DC    H'0'                                                             
         EJECT                                                                  
VALP74   GOTO1 AFVAL,PKGUPDH       VALIDATE UPDATE FIELD                        
         BH    VALPX                                                            
         BL    VALP88                                                           
         TM    FVIIND,FVITHIS                                                   
         BZ    VALP88                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VALP88                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   ENOTV                                                            
         LA    R3,IOKEY            UPDATE PACKAGE RECS FROM WORK RECS           
         MVC   IOKEY(13),APRECKEY                                               
         MVC   BWDKELPO,LPKGNUM                                                 
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         MVI   BWDKELSQ,1                                                       
         LA    R1,MINHI2                                                        
         B     VALP76+4                                                         
*                                                                               
VALP76   LA    R1,MINSEQ2          READ ALL PACKAGE RECORDS                     
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALP88                                                           
         CLC   IOKEY(BWDKELDY-BWDKEY),IOKEYSAV                                  
         BNE   VALP88                                                           
         L     R3,AIOAREA2                                                      
         CLI   BWDKELSQ,0          CHECK IT'S A PACKAGE SLAVE                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   BDPT,BWDDPT         SAVE SOME RECORD DETAILS                     
         MVC   BSLN,BWDSLN                                                      
         MVC   BDAYS,BWDPODAY                                                   
         GOTO1 APACKTIM,BWDTIMES                                                
         MVC   APFULL,BWDCOST1                                                  
         MVC   LUCODE,BWDUCODE                                                  
         GOTO1 AGETRTG             GET THE TARGET RATING                        
         MVC   LOLDRTG,LRATING                                                  
*                                                                               
         LA    R3,IOKEY                                                         
         MVC   IOKEY(13),APRECKEY  READ WORK RECORDS TO MATCH PACKAGE           
         MVI   BWDKELPO,0          RECORD                                       
         MVC   BWDKELDY,BDAYS                                                   
         MVC   BWDKELTM,PTIMES                                                  
         MVI   BWDKELSQ,0                                                       
         MVI   APBYTE,0                                                         
         LA    R1,MINHI3                                                        
         B     VALP78+4                                                         
*                                                                               
VALP78   LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     *+14                                                             
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BE    VALP80                                                           
         CLI   APBYTE,0                                                         
         BE    EWNF                                                             
         LA    R3,IOKEY                                                         
         MVC   IOKEY,IOKEYSAV                                                   
         MVC   BWDKELSQ,APBYTE                                                  
         GOTO1 AMIN,MINRD3                                                      
         BE    VALP82                                                           
         DC    H'0'                                                             
*                                                                               
VALP80   L     R3,AIOAREA3                                                      
         CLC   BWDDPT,BDPT         MATCH DAYPART AND LENGTH                     
         BNE   VALP78                                                           
         CLC   BWDSLN,BSLN                                                      
         BNE   VALP78                                                           
         CLC   LUCODE,SPACES       MATCH USER CODE, IF ANY                      
         BNH   *+18                                                             
         CLC   BWDUCODE,LUCODE                                                  
         BNE   VALP78                                                           
         B     VALP82                                                           
         CLC   BWDCOST1,APFULL     MATCHED-TEST COSTS MATCH                     
         BE    VALP82              YES                                          
         CLI   APBYTE,0            NO-TEST ONE RECORD FOUND YET                 
         BNE   VALP78                                                           
         MVC   APBYTE,BWDKELSQ     NO-SAVE FIRST RECORD SEQ NUM                 
         B     VALP78                                                           
*                                                                               
VALP82   L     R3,AIOAREA2         WORK RECORD FOUND -                          
         SR    R0,R0               SAVE SPOTS/WEEK AND TRANSFER ELEMS           
         LA    R9,BWDEL                                                         
         XC    LSPWEL,LSPWEL                                                    
         XC    LBTREL,LBTREL                                                    
*                                                                               
VALP83   CLI   0(R9),0                                                          
         BE    VALP86                                                           
         LA    R1,LSPWEL                                                        
         CLI   0(R9),SPWELCDQ                                                   
         BE    VALP84                                                           
         LA    R1,LBTREL                                                        
         CLI   0(R9),BTRELCDQ                                                   
         BNE   VALP85                                                           
*                                                                               
VALP84   ZIC   RE,1(R9)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     VALP85                                                           
         MVC   0(0,R1),0(R9)                                                    
*                                                                               
VALP85   IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     VALP83                                                           
*                                                                               
VALP86   LA    R0,BWDFSTEL+BWDELLNQ+1-BWDRECD                                   
         STCM  R0,3,BWDLEN                                                      
         XC    BWDEL(256),BWDEL    CLEAR PACKAGE RECORD AFTER KEY               
         GOTO1 ABLDPKG             BUILD ELEMENTS FROM WORK RECORD              
         LA    R1,LSPWEL           ADD BACK SAVED ELEMENTS                      
         BAS   RE,ADDELEM                                                       
         LA    R1,LBTREL                                                        
         BAS   RE,ADDELEM                                                       
         CLI   LSPWEL,0            TEST SPOTS/WEEK ELEMENT                      
         BE    VALP87                                                           
         GOTO1 AGETRTG             YES-GET TARGET RATING                        
         CLC   LRATING,LOLDRTG         TEST RATING CHANGE                       
         BE    VALP87                                                           
         GOTO1 AACTPTS                 YES-CALCULATE POINTS CHANGE              
*                                                                               
VALP87   MVC   IOKEY(13),BWDKEY                                                 
         GOTO1 AMIN,MINRD3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AMIN,MINWRT2        PUT UPDATED PACKAGE RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AMIN,MINHI2                                                      
         B     VALP76              READ SEQUENTIAL PACKAGE RECORDS              
         EJECT                                                                  
VALP88   CLI   INREC,RECPKG        FOR PACKAGE RECORDS --                       
         BNE   VALP93                                                           
         L     R1,ALSM                                                          
         USING LSMD,R1                                                          
         TM    LSMINDS,LSMISEL     IF USER NOW INVITED TO ENTER                 
         BZ    VALP93              SELECTIONS,                                  
         CLC   APRECKEY,LSMUKEY    AND THE KEY HAS NOT CHANGED --               
         BNE   VALP93                                                           
         DROP  R1                                                               
         ZIC   R0,LNLINES                                                       
         SR    R1,R1                                                            
         LA    RE,PKGDT1H                                                       
         LA    R4,PKGL1H                                                        
         USING PKGL1H,R4                                                        
*                                                                               
VALP89   LR    RF,RE               FOR ALL SCHEDULE LINES --                    
         AH    RF,=Y(PKGDT2H-PKGDT1H)                                           
*                                                                               
VALP90   CR    RE,RF               SCAN THE FIELDS                              
         BE    VALP92                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
         TM    FVATRB-FVIHDR(RE),FVAPROT    TEST FOR UNPROTECTED                
         BO    VALP91                                                           
         TM    FVIIND-FVIHDR(RE),FVIVAL     TEST FOR NOT PREV VALIDATED         
         BO    VALP91                                                           
         NI    PKGCOSH+FVIIND-FVIHDR,FF-FVIVAL TURN OFF PREV VALIDATED          
         B     VALP92                          BIT IN CORRESP LIST LINE         
*                                                                               
VALP91   ICM   R1,1,0(RE)          NEXT FIELD                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R1                                                            
         B     VALP90                                                           
*                                                                               
VALP92   LR    RE,RF               NEXT SCHEDULE LINE                           
         LA    R4,PKGL2H-PKGL1H(R4)                                             
         BCT   R0,VALP89                                                        
         DROP  R4                                                               
*                                                                               
VALP93   TM    TWAFLAG,TWAFMTPD    TEST SHOULD FORMAT ACTUAL PTS/DOL            
         BZ    VALP94                                                           
******   GOTO1 AFMACPTS,APPARM,PKGACTH,SVPKGVA                                  
         LR    RE,R5                                                            
         AHI   RE,SVPKGVA-TWAD                                                  
         ST    RE,APPARM+4                                                      
         GOTO1 AFMACPTS,APPARM,PKGACTH,,0                                       
*                                                                               
******   GOTO1 AFMACDOL,(R1),PKGACTH,SVPKGVA                                    
         LR    RE,R5                                                            
         AHI   RE,SVPKGVA-TWAD                                                  
         ST    RE,APPARM+4                                                      
         GOTO1 AFMACDOL,(R1),PKGACTH,,0                                         
*                                                                               
         TM    TWAINDS,TWAISKD     TEST COME FROM SKED SCREEN                   
         BO    VALP94                                                           
         NI    TWAFLAG,255-TWAFMTPD     NO-TURN OFF FLAG NOW                    
*                                                                               
VALP94   LA    R0,PKGL1H       SET VALUES FOR ROOT                              
         ST    R0,APPARM+0                                                      
         MVC   APPARM+4(1),LNLINES NUMBER OF LIST LINES                         
         LA    R1,PKGL2H                                                        
         CLI   INREC,RECPKG                                                     
         BE    *+8                                                              
         LA    R1,ORBL2H                                                        
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALPX                                                            
*                                                                               
VALP97   MVC   FVMSGNO,=AL2(FVNODPT)    DAYPART MISSING                         
         B     VALP98+6                                                         
*                                                                               
VALP98   MVC   FVMSGNO,=AL2(FVNOSLN)    SPOT LENGTH MISSING                     
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     VALPX                                                            
*                                                                               
VALP99   MVC   FVMSGNO,=AL2(FVFNOTN)    NOT NUMERIC                             
         B     VALPX                                                            
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST LIST SCREEN HOOK                                              *         
***********************************************************************         
         SPACE 1                                                                
FSTSCR   L     R3,AIOAREA1         R3=A(MASTER RECORD)                          
         CLI   INREC,RECPKG        FOR PACKAGE SCREEN --                        
         BNE   FSCR8                                                            
         TM    TWAINDS,TWAISKD     TEST COME FROM SKED SCREEN                   
         BO    *+8                                                              
         NI    TWAFLAG,255-TWAFMTPD  NO-TURN OFF FORMAT ACTUAL PTS/DOL          
         LR    R2,R5               FORMAT THE SCHEDULE CALENDAR                 
         AHI   R2,CMPDATSD-TWAD                                                 
         LA    R8,PKGCA1H                                                       
         LA    R4,L'FVIHDR(R8)                                                  
         LA    R8,L'FVIHDR-1(R8)                                                
         LA    R9,PKGCA2H+L'FVIHDR-1                                            
*                                                                               
FSCR2    MVC   2(2,R9),4(R2)       DAY NUMBER                                   
         LA    R8,4(R8)                                                         
         LA    R9,4(R9)                                                         
         CLI   6(R2),FF            TEST END OF CAMPAIGN PERIOD                  
         BE    FSCR4                                                            
         CLC   2(2,R2),8(R2)       TEST CHANGE OF MONTH                         
         BNE   FSCR4                                                            
         LA    R2,6(R2)                                                         
         B     FSCR2                                                            
*                                                                               
FSCR4    LR    RE,R8               FORMAT MONTH NAME TO LINE ABOVE              
         SR    RE,R4                                                            
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         MVI   0(R4),C'-'                                                       
         EX    RE,*+4                                                           
         MVC   1(0,R4),0(R4)                                                    
         LA    RE,3(RE)                                                         
         SRL   RE,1                                                             
         AR    R4,RE                                                            
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         GOTO1 VDATCON,APPARM,(0,(R2)),(4,APWORK)                               
         MVC   0(3,R4),APWORK                                                   
         CLI   6(R2),FF            TEST END OF CAMPAIGN PERIOD                  
         BE    FSCR6               YES                                          
         LA    R2,6(R2)            NO - CONTINUE                                
         LA    R4,1(R8)                                                         
         B     FSCR2                                                            
*                                                                               
FSCR6    OI    PKGCA1H+6,FVOXMT    TRANSMIT                                     
         OI    PKGCA2H+6,FVOXMT                                                 
*                                                                               
FSCR8    LA    R0,4                FORMAT DEMOS NAMES TO HEADLINE               
         LA    R4,PKGDN1H                                                       
         LA    R8,COMDNAMS                                                      
*                                                                               
FSCR10   OC    0(6,R8),0(R8)                                                    
         BZ    FSCR12                                                           
         MVC   FVIFLD-FVIHDR(L'PKGDN1,R4),0(R8)                                 
         OI    6(R4),FVOXMT                                                     
         LA    R4,PKGDN2H-PKGDN1H(R4)                                           
         LA    R8,6(R8)                                                         
         BCT   R0,FSCR10                                                        
*                                                                               
FSCR12   TM    LFLAG,LNEWPKG       TEST NEW PACKAGE                             
         BZ    FSCR40                                                           
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PKGDAYH       YES - MAKE SURE AT LEAST ONE DETAIL          
         BNE   FSCR99                                                           
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PKGTIMH                                                    
         BNE   FSCR99                                                           
         LA    R4,PKGL1H           VALIDATE PACKAGE DETAIL LINES                
         USING PKGL1H,R4                                                        
         ZIC   R8,LNLINES          R8 = LINES/SCREEN                            
         LA    R9,LRECTAB                                                       
         XC    LRECTAB,LRECTAB                                                  
         MVC   LSVDPT,BDPT         SAVE DAYPART/LENGTH                          
         MVC   LSVSLN,BSLN                                                      
*                                                                               
FSCR14   CLI   PKGL1,C'*'          TEST IGNORE THIS LINE                        
         BE    FSCR26                                                           
         L     R3,AIOAREA1                                                      
         GOTO1 AVALDAY,PKGDAYH     DAYS                                         
         BE    *+18                                                             
         CLC   FVMSGNO,=AL2(FVFNONE)   TEST DAYS NOT FOUND                      
         BE    FSCR28              YES - ASSUME END OF LIST                     
         B     FSCR99                                                           
         GOTO1 AVALTIM,PKGTIMH     TIMES                                        
         BNE   FSCR99                                                           
*                                  DAYPART                                      
         MVC   BDPT,BWDDPT         FOR ORBITS, DPT=ORBIT MASTER DPT             
         CLI   INREC,RECPKG        TEST PACKAGE RECORD                          
         BNE   FSCR16                                                           
         GOTO1 AVALDPL,PKGDPTH     YES-VALIDATE DAYPART                         
         BE    FSCR16                                                           
         CLC   FVMSGNO,=AL2(FVFNONE)   TEST DAYPART MISSING                     
         BNE   FSCR99                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVC   BDPT,BWDDPT         YES-DEFAULT IS PACKAGE MASTER DPT            
*                                                                               
FSCR16   MVC   BSLN,BWDSLN         SPOT LENGTH=PACKAGE/ORBIT MASTER SLN         
         LA    R1,PKGCOSH          COST                                         
         CLI   INREC,RECPKG                                                     
         BE    *+8                                                              
         LA    R1,ORBCOSH                                                       
         GOTO1 AFVAL                                                            
         BH    FSCR99                                                           
         BL    FSCR18                                                           
         MVC   LUCODE,SPACES                                                    
         CLC   FVIFLD(2),=C'U='    TEST FOR USER CODE                           
         BNE   FSCR17                                                           
         MVC   LUCODE,FVIFLD+2     YES                                          
         CLC   LUCODE,SPACES                                                    
         BE    FSCR95                                                           
         B     FSCR18                                                           
*                                                                               
FSCR17   ZIC   RE,FVILEN           ELSE COST                                    
         ST    RE,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   0(R1),0                                                          
         BNE   FSCR97                                                           
         GOTO1 ANETCOST,APPARM+4   NET DOWN THE COST IF NECESSARY               
         MVC   4(4,R9),APPARM+4    SAVE COST IN RECORD TABLE                    
*                                                                               
FSCR18   MVC   IOKEY(13),APRECKEY  READ WORK RECORDS TO MATCH THIS LINE         
         LA    R3,IOKEY                                                         
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,BDAYS                                                   
         MVC   BWDKELTM,PTIMES                                                  
         MVI   BWDKELSQ,0                                                       
         LA    R1,MINHI3                                                        
         B     FSCR20+4                                                         
*                                                                               
FSCR20   LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     *+14                                                             
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BE    FSCR22                                                           
         OC    0(4,R9),0(R9)       TEST WORK RECORD MATCH                       
         BNZ   FSCR26                                                           
         LA    R1,PKGL1H           NO - ERROR EXIT                              
         ST    R1,FVADDR                                                        
         B     FSCR98                                                           
*                                                                               
FSCR22   L     R3,AIOAREA3                                                      
         CLC   BWDDPT,BDPT         MATCH DAYPART/LENGTH                         
         BNE   FSCR20                                                           
         CLC   BWDSLN,BSLN                                                      
         BNE   FSCR20                                                           
         CLI   FVILEN,0            MATCHED-TEST COST INPUT                      
         BNE   FSCR24                                                           
         OC    0(4,R9),0(R9)       NO-TEST ALREADY ONE MATCH                    
         BNZ   *+20                                                             
         MVC   0(4,R9),BWDKELDY    NO-SAVE THIS RECORD DETAILS                  
         MVC   4(4,R9),BWDCOST1                                                 
         B     FSCR20                                                           
         CLC   BWDCOST1,4(R9)      YES-TEST SAME COST AS BEFORE                 
         BNE   FSCR96              NO-COST IS MISSING INPUT FIELD               
         B     FSCR20                                                           
*                                                                               
FSCR24   CLC   LUCODE,SPACES       TEST USER CODE INPUT                         
         BNH   FSCR25                                                           
         CLC   BWDUCODE,LUCODE     YES-TRY TO MATCH USER CODE                   
         BNE   FSCR20                                                           
         MVC   0(4,R9),BWDKELDY                                                 
         MVC   4(4,R9),BWDCOST1                                                 
         B     FSCR26                                                           
*                                                                               
FSCR25   OC    0(4,R9),0(R9)       COST WAS INPUT-TEST ALREADY MATCHED          
         BNZ   *+10                                                             
         MVC   0(4,R9),BWDKELDY    NO-SAVE RECORD DETAILS                       
         CLC   BWDCOST1,4(R9)      TEST THIS RECORD HAS THE COST                
         BNE   FSCR20              NO-CONTINUE READING                          
         MVC   0(4,R9),BWDKELDY    YES-SAVE THIS RECORD DETAILS                 
*                                                                               
FSCR26   CLI   INREC,RECPKG             NEXT LINE                               
         BNE   *+12                                                             
         LA    R4,PKGL2H-PKGL1H(R4)                                             
         B     *+8                                                              
         LA    R4,ORBL2H-ORBL1H(R4)                                             
         LA    R9,8(R9)                                                         
         BCT   R8,FSCR14                                                        
         DROP  R4                                                               
*                                                                               
FSCR28   MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   BDPT,LSVDPT         RESTORE DAYPART/LENGTH                       
         MVC   BSLN,LSVSLN                                                      
         LA    R4,PKGL1H           NOW ADD PACKAGE LINE RECORDS                 
         USING PKGL1H,R4                                                        
         LA    R2,1                R2 = PACKAGE LINE COUNT                      
         ZIC   R8,LNLINES          R8 = LINES/SCREEN                            
         LA    R9,LRECTAB                                                       
*                                                                               
FSCR30   OC    0(4,R9),0(R9)       TEST IGNORE THIS LINE                        
         BZ    FSCR32                                                           
         MVC   IOKEY(13),APRECKEY                                               
         LA    R3,IOKEY                                                         
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY(4),0(R9)                                                
         GOTO1 AMIN,MINRD3         GET WORK RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2         BUILD PKG LINE                               
         XC    BWDRECD(256),BWDRECD                                             
         MVC   BWDKEY,IOKEY                                                     
         MVC   BWDKELPO,LPKGNUM    PACKAGE NUMBER                               
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         STC   R2,BWDKELSQ         PACKAGE SLAVE SEQUENCE NUMBER                
         LA    R0,BWDEL+BWDELLNQ+1-BWDRECD                                      
         STCM  R0,3,BWDLEN                                                      
         MVC   APFULL,4(R9)        COST                                         
         GOTO1 ABLDPKG             BUILD ELEMENTS FROM WORK RECORD              
         GOTO1 ADISPKG             DISPLAY PACKAGE LINE                         
         GOTO1 AMIN,MINADD2        ADD PACKAGE LINE                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,1(R2)            BUMP LINE COUNT                              
*                                                                               
FSCR32   CLI   INREC,RECPKG             NEXT LINE                               
         BNE   *+12                                                             
         LA    R4,PKGL2H-PKGL1H(R4)                                             
         B     *+8                                                              
         LA    R4,ORBL2H-ORBL1H(R4)                                             
         LA    R9,8(R9)                                                         
         BCT   R8,FSCR30                                                        
*                                  NOW ADD PACKAGE HEADER                       
         GOTO1 AMIN,MINADD1                                                     
         BE    FSCR40                                                           
         DC    H'0'                                                             
*                                                                               
FSCR40   CLI   INREC,RECPKG        TEST PACKAGE                                 
         BNE   FSCRX                                                            
         GOTO1 AGETGVA             YES-GET AND FORMAT GOAL VS ACTUAL            
         B     FSCRX                                                            
*                                                                               
FSCR95   MVC   FVMSGNO,=AL2(FVFNOTV)    INVALID INPUT FIELD                     
         B     FSCR99                                                           
*                                                                               
FSCR96   MVC   FVMSGNO,=AL2(FVFNONE)    MISSING INPUT FIELD                     
         B     FSCR99                                                           
*                                                                               
FSCR97   MVC   FVMSGNO,=AL2(FVICST)     INVALID COST                            
         B     FSCR99                                                           
*                                                                               
FSCR98   MVC   FVMSGNO,=AL2(FVIWKREC)   WORK RECORD NOT FOUND                   
*                                                                               
FSCR99   L     R1,ALSM             ERROR XIT - TELL CONTROLLER THAT WE          
         OI    LSMINDS-LSMD(R1),LSMIEOL        HAVE END-OF-LIST                 
         B     FSCRX                                                            
*                                                                               
FSCRX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR LIST SCREEN HOOK                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   CLI   SCPFKEY,255         TEST ANY LFM ACTION FROM THIS SCREEN         
         BE    LSCR2                                                            
         MVC   SCPFKEY,APPFKEY     SET PF KEY                                   
         TM    SVACTIV2,SVVALSEL   TEST FOR ANY VALIDATE LIST/SELECT            
         BZ    LSCRX                                                            
         CLI   APPFKEY,0           YES - TEST FOR NO PF KEY                     
         BNE   LSCRX                                                            
*                                                                               
LSCR2    MVI   SCPFKEY,PFK05       FORCE STAY ON CURRENT SCREEN                 
         MVI   APMODE,APMPFKS      PF KEY SET                                   
         NI    SVACTIV2,FF-SVVALSEL                                             
*                                                                               
LSCRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
PUTKEY   LA    R4,APELEM                                                        
         SR    R0,R0                                                            
         MVI   0(R4),KEYMED                                                     
         MVI   1(R4),3                                                          
         MVC   2(1,R4),QMED                                                     
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYBYR                                                     
         MVI   1(R4),5                                                          
         MVC   2(3,R4),QBYR                                                     
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYCAM                                                     
         LA    R1,QCAM                                                          
         LA    RE,4                                                             
         CLI   0(R1),C'0'                                                       
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         EX    RE,*+4                                                           
         MVC   2(0,R4),0(R1)                                                    
         LA    RE,3(RE)                                                         
         STC   RE,1(R4)                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYMKT                                                     
         MVI   1(R4),6                                                          
         MVC   2(4,R4),APRECDA                                                  
         CLI   APRECDA,C'0'        TEST CABLE                                   
         BL    *+18                                                             
         MVI   6(R4),C'/'          YES                                          
         MVC   7(3,R4),APRECKEY+40                                              
         MVI   1(R4),10                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYDPL                                                     
         MVI   1(R4),6                                                          
         MVC   2(1,R4),APRECID+1                                                
         ZIC   RE,APRECID+2                                                     
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  3(3,R4),APDUB                                                    
         CLI   3(R4),C'0'                                                       
         BNE   *+14                                                             
         MVC   3(2,R4),4(R4)                                                    
         MVI   1(R4),5                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),0                                                          
         GOTO1 APUTKEY                                                          
PUTKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
* INPUT  : APINDS EQ APILFLST FIRST TIME / FIRST SCREEN               *         
*                    APILNLST FISRT TIME / NOT FIRST SCREEN           *         
*                    APILNSEQ NEXT TIME(S)                            *         
*          APRECKEY                                                   *         
* OUTPUT : APRECKEY                                                   *         
*          APRECDA                                                    *         
*          APRECNUM = RECPKG                                          *         
*          APMODE  = APMEOFS FOR END-OF-LIST                          *         
*          FOR DUMMY RECORD TO FILL REST OF SCREEN :                  *         
*          APRECKEY(1) = X'FF' AND APRECNUM = X'FF'                   *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    RE,NMAXWKS          CLEAR SPOTS/WEEK                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    APRECKEY+20(0),APRECKEY+20                                       
         CLI   APINDS,APILNSEQ     TEST FIRST LINE ON SCREEN                    
         BNE   GETS2                                                            
         ZIC   RE,LINE             NO - AUGMENT LINE NUMBER                     
         LA    RE,1(RE)                                                         
         STC   RE,LINE                                                          
         CLI   APRECKEY,FF         TEST ALREADY PASSING DUMMY RECORDS           
         BE    GETS24              YES                                          
         LA    R1,MINSEQ2          NO - READ SEQUENTIAL                         
         B     GETS6                                                            
*                                                                               
GETS2    MVI   LINE,1              TOP LINE - RESET LINE NUMBER                 
         MVI   LASTLINE,FF                    INIT FIRST DUMMY LINE NUM         
         MVI   SVACTIV2,0                     NO ACTIVITY YET                   
         TM    APINDS,APILFLST     TEST FIRST SCREEN / FIRST LINE               
         BO    *+16                                                             
         CLI   APRECKEY,FF         NO-TEST FIRST RECORD IS DUMMY                
         BE    GETS24                                                           
         B     GETS4                                                            
         MVC   BWDKELPO,LPKGNUM    FIRST SCREEN-SET THE KEY                     
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         MVI   BWDKELSQ,1                                                       
*                                                                               
GETS4    LA    R1,MINHI2           READ HIGH                                    
*                                                                               
GETS6    MVC   IOKEY(13),APRECKEY                                               
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETS24                                                           
         CLC   IOKEY(BWDKELDY-BWDKEY),IOKEYSAV                                  
         BNE   GETS24                                                           
         XC    APRECDA,APRECDA     RECORD FOUND - SET VALUES FOR ROOT           
         MVI   APRECID,0                                                        
         MVC   APRECNUM,INREC                                                   
         MVC   APRECKEY(13),IOKEY                                               
*                                                                               
         CLI   INREC,RECPKG        FOR PACKAGE RECORDS --                       
         BNE   GETSX                                                            
         LA    R2,APRECKEY+20                                                   
         L     R3,AIOAREA2                                                      
         SR    R0,R0               BUILD SPOTS/WEEK GRID                        
         LA    R1,BWDEL                                                         
*                                                                               
GETS8    CLI   0(R1),0                                                          
         BE    GETS10                                                           
         CLI   0(R1),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETS8                                                            
         USING SPWEL,R1                                                         
         ZIC   R9,SPWELLN                                                       
         AR    R9,R1                                                            
         BCTR  R9,0                                                             
         LA    R8,1                                                             
         LA    R1,SPWPERWK                                                      
         DROP  R1                                                               
*                                                                               
         LR    RE,R2                                                            
         MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         BXLE  R1,R8,*-10                                                       
*                                                                               
GETS10   TM    INOIND,INOINOG      TEST NOGOALS OPTION                          
         BO    GETS16                                                           
         TM    CLTIND,CLTIGOL      NO - TEST GOALS REQUIRED FOR BUY             
         BZ    GETS16                                                           
         CLC   TWADPT,BWDDPT       YES-MARK AS INVALID THOSE WEEKS              
         BNE   *+14                    WITHOUT GOALS                            
         CLC   TWASLN,BWDSLN                                                    
         BE    GETS12                                                           
         MVC   LSVDPT,BDPT                                                      
         MVC   LSVSLN,BSLN                                                      
         MVC   BDPT,BWDDPT                                                      
         CLI   BWDSUBDP,0                                                       
         BE    *+10                                                             
         MVC   BDPT,BWDSUBDP                                                    
         XC    DPTSUBS,DPTSUBS                                                  
         MVC   BSLN,BWDSLN                                                      
         GOTO1 AGETGOAL                                                         
         GOTO1 AGETDPT,LSVDPT                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDPT,LSVDPT                                                      
         MVC   BSLN,LSVSLN                                                      
*                                                                               
GETS12   ZIC   R0,CMPNWKS                                                       
         L     RE,AIOAREA3                                                      
         LA    RE,64(RE)                                                        
         LR    RF,R2                                                            
*                                                                               
GETS14   OC    0(4,RE),0(RE)                                                    
         BNZ   *+8                                                              
         OI    0(RF),X'40'                                                      
         LA    RE,4(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GETS14                                                        
*                                                                               
GETS16   LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         ST    RE,APPARM                                                        
*                                                                               
         GOTO1 VGETDAY,APPARM,,APWORK GET DAY OF WEEK OF FIRST                  
         CLC   APWORK(3),SPACES               DAY OF CAMPAIGN                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,0(R1)                                                     
         SR    R8,R8                                                            
         ICM   R8,1,ESTOWSDY       TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS17                                                           
         BCTR  R8,0                YES-GET RELATIVE DAY                         
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APBYTE                                                        
         CLI   APBYTE,1            TEST FIRST DAY OF WEEK                       
         BE    GETS17A             YES-OK                                       
*                                                                               
GETS17   ZIC   RE,BWDPODAY         GET DAY OF WEEK OF LAST DAY OF DAYS          
         SR    RF,RF                                                            
         LA    R1,7                                                             
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    *+6                                                              
         SR    R1,R8                                                            
         SRDL  RE,1                                                             
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BCT   R1,*-10                                                          
         CLM   R1,1,APBYTE         TEST LAST DAY BEFORE CAMPAIGN START          
         BNL   *+8                                                              
         OI    0(R2),X'80'         YES - MARK FIRST WEEK AS INVALID             
*                                                                               
GETS17A  MVC   APWORK(6),CMPFLND   GET DAY OF WEEK OF LAST DAY OF               
         TM    CMPOPTS,CAMOWKS     CAMPAIGN                                     
         BO    GETS18                                                           
         GOTO1 VDATCON,APPARM,(3,CMPND),(0,APWORK)                              
*                                                                               
GETS18   GOTO1 VGETDAY,APPARM,APWORK,APWORK+6                                   
         CLC   APWORK+6(3),SPACES                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,0(R1)                                                     
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS19                                                           
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APBYTE                                                        
*                                                                               
GETS19   SR    RE,RE               GET DAY OF WEEK OF FIRST DAY OF DAYS         
         ZIC   RF,BWDPODAY                                                      
         SLL   RF,25                                                            
         SR    R1,R1                                                            
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    *+10                                                             
         SR    R1,R8                                                            
         LA    R1,7(R1)                                                         
         LA    R1,1(R1)                                                         
         SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    *-10                                                             
         CLM   R1,1,APBYTE         TEST FIRST DAY AFTER CAMPAIGN END            
         BNH   GETS20                                                           
         ZIC   RF,CMPNWKS          YES - MARK LAST WEEK AS INVALID              
         AR    RF,R2                                                            
         BCTR  RF,0                                                             
         OI    0(RF),X'40'                                                      
*                                                                               
GETS20   STC   R1,APHALF           APHALF(1) = FIRST DAY OF DAYS                
         GOTO1 VGETDAY,APPARM,ESTST,APWORK    GET DAY OF WEEK OF FIRST          
         CLC   APWORK(3),SPACES               DAY OF ESTIMATE                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHALF+1(1),0(R1)              =APHALF+1(1)                      
         LTR   R8,R8                                                            
         BZ    GETS21                                                           
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APHALF+1                                                      
*                                                                               
GETS21   MVC   APWORK(6),ESTST                                                  
         CLI   0(R1),1             GET MONDAY OF EST START WEEK                 
         BE    GETS22                                                           
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         LNR   RE,RE                                                            
         ST    RE,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,ESTST,APWORK                                       
*                                                                               
GETS22   MVC   APWORK+6(6),CMPSTMON    CAMPAIGN START MONDAY                    
         TM    CMPOPTS,CAMOWKS         TEST NON-CONTIGUOUS FLIGHT WEEKS         
         BZ    *+10                                                             
         MVC   APWORK+6(6),CMPFLSTM    YES-USE FLIGHT START MONDAY              
         CLC   APWORK+6(6),APWORK  TEST CAMPAIGN START WK=EST START WK          
         BNE   GETS23                                                           
         CLC   APHALF(1),APHALF+1  TEST FIRST DAY BEFORE EST START              
         BNL   GETS23                                                           
         OI    0(R2),X'80'         YES - FIRST WEEK IS INVALID                  
*                                                                               
GETS23   TM    LFLAG,LDATES        TEST PACKAGE MASTER HAS DATES                
         BZ    GETSX                                                            
         L     R1,AIOAREA1         YES-                                         
         CLC   BWDDATES,BWDDATES-BWDRECD(R1) TEST SAME AS SLAVE DATES           
         BE    GETSX                                                            
         MVC   BWDDATES,BWDDATES-BWDRECD(R1) NO-CHANGE THEM TO MASTER'S         
         MVC   BWDWKS,BWDWKS-BWDRECD(R1)        DATES                           
         GOTO1 AMIN,MINWRT2                  AND WRITE THE RECORD BACK          
         BE    GETSX                                                            
         DC    H'0'                                                             
*                                                                               
GETS24   MVI   APRECKEY,FF         DUMMY RECORD                                 
         MVI   APRECNUM,FF                                                      
         CLC   LINE,LASTLINE       TEST FIRST DUMMY LINE                        
         BNL   *+10                                                             
         MVC   LASTLINE,LINE       YES - SAVE FIRST DUMMY LINE NUM              
         CLC   LINE,LNLINES        TEST LAST SCREEN LINE                        
         BNH   GETSX                                                            
         CLC   LASTLINE,LNLINES    YES                                          
         BH    GETSX                                                            
         MVI   APMODE,APMEOFS      END-OF-LIST                                  
         B     GETSX                                                            
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                           *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   CLI   APRECKEY,FF         TEST DUMMY RECORD                            
         BE    DISS2               YES - DISPLAY NOTHING                        
         MVC   IOKEY(13),APRECKEY  NO - GET THE RECORD                          
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2                                                      
         L     R4,APPARM                                                        
         GOTO1 ADISPKG             DISPLAY THE RECORD                           
*                                                                               
DISS2    CLI   INREC,RECPKG        FOR PACKAGE RECORDS ONLY --                  
         BNE   DISSX                                                            
         ZIC   RF,LINE                                                          
         BCTR  RF,0                                                             
         LTR   RF,RF               TEST FIRST LINE ON SCREEN                    
         BNZ   DISS6                                                            
         ZIC   R0,LNLINES          YES - CLEAR SCHEDULE LINES                   
         LA    R4,PKGDT1H                                                       
         USING PKGDT1H,R4                                                       
*                                                                               
DISS4    XC    PKGDT1,PKGDT1                                                    
         OI    PKGDT1H+6,FVOXMT                                                 
         NI    PKGDT1H+FVATRB-FVIHDR,FF-FVAHIGH                                 
         XC    PKGSKD,PKGSKD                                                    
         OI    PKGSKDH+6,FVOXMT                                                 
         OI    PKGSKDH+FVIIND-FVIHDR,FVIVAL TURN ON PREV VALIDATED BIT          
         NI    PKGSKDH+FVATRB-FVIHDR,FF-FVAHIGH                                 
         LA    R4,PKGDT2H-PKGDT1H(R4)                                           
         BCT   R0,DISS4                                                         
         DROP  R4                                                               
*                                                                               
DISS6    CLI   APRECKEY,FF         TEST DUMMY RECORD                            
         BE    DISSX                                                            
         LA    R2,APRECKEY+20      R2 = A(SPOTS/WEEK GRID)                      
         MH    RF,=Y(PKGDT2H-PKGDT1H)                                           
         LA    R4,PKGDT1H(RF)      R4 = A(CURRENT SCHEDULE TWA LINE)            
         USING PKGDT1H,R4                                                       
*                                                                               
         LA    R8,PKGDT1           NO - FORMAT LINE/DAYS/TIMES                  
         USING SKDLINED,R8                                                      
         MVC   SLLINE,LDSPLINE     LINE NUM                                     
         MVC   SLDAYS,QDAYS        DAYS                                         
         MVC   SLTIMES,QTIMES      TIMES                                        
         OI    PKGDT1H+6,FVOXMT                                                 
         DROP  R8                                                               
*                                                                               
DISS8    MVC   PKGSKD,SPACES       FORMAT THE SCHEDULE                          
         LA    R8,PKGSKD+1                                                      
         ZIC   RE,CMPNWKS                                                       
         SR    R1,R1                                                            
         ICM   R1,12,BWDWKS                                                     
*                                                                               
DISS10   SR    R0,R0                                                            
         SLDL  R0,1                                                             
         MVI   1(R8),C'.'                                                       
         TM    0(R2),X'C0'         TEST WEEK OUT OF BOUNDS                      
         BNZ   *+10                                                             
         LTR   R0,R0               TEST INACTIVE WEEK                           
         BZ    *+12                                                             
         MVI   1(R8),C'X'          YES - MARK WITH X                            
         B     DISS12                                                           
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          TEST ANY SPOTS THIS WEEK                     
         BZ    DISS12                                                           
         CVD   RF,APDUB            YES - FORMAT SPOTS                           
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(2,R8),APDUB                                                    
*                                                                               
DISS12   LA    R2,1(R2)            NEXT SCHEDULE WEEK                           
         LA    R8,4(R8)                                                         
         BCT   RE,DISS10                                                        
*                                                                               
DISSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT  : APPARM+0(4) = A(TWA DISPLAY LINE)                          *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   L     R3,AIOAREA2                                                      
         MVC   COMATWAL,APPARM     SAVE A(TWA LINE)                             
         CLI   APRECNUM,FF         TEST DUMMY RECORD                            
         BE    VALS2                                                            
         MVC   IOKEY(13),APRECKEY  NO-READ THE PACKAGE RECORD                   
         GOTO1 AMIN,MINRD2                                                      
         BE    VALS8                                                            
         MVC   FVADDR,APPARM                                                    
         B     VALSX                                                            
*                                                                               
VALS2    OI    LFLAG,LNEWLINE         DUMMY - INDICATE NEW LINE                 
         XC    BWDRECD(256),BWDRECD   BUILD NEW RECORD                          
         MVC   BWDKEY,APRECKEY                                                  
         MVI   BWDKTYP,BWDKTYPQ                                                 
         L     R1,AIOAREA1         PACKAGE NUMBER FROM PACKAGE MASTER           
         MVC   BWDKELPO,BWDKELPO-BWDKEY(R1)                                     
         MVC   IOKEY(13),BWDKEY    FIND NEXT AVAILABLE SEQ NUM                  
         LA    R3,IOKEY                                                         
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         MVI   BWDKELSQ,1          FORCE PAST PACKAGE MASTER                    
         SR    R8,R8                                                            
         LA    R1,MINHI3                                                        
         B     VALS4+4                                                          
*                                                                               
VALS4    LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALS6                                                            
         CLC   IOKEY(BWDKELDY-BWDKEY),IOKEYSAV                                  
         BNE   VALS6                                                            
         CLM   R8,1,BWDKELSQ                                                    
         BNL   VALS4                                                            
         IC    R8,BWDKELSQ                                                      
         B     VALS4                                                            
*                                                                               
VALS6    LA    R8,1(R8)                                                         
         L     R3,AIOAREA2                                                      
         STC   R8,BWDKELSQ                                                      
*                                                                               
VALS8    NI    LFLAG,FF-LCHG       SCAN TWA LINE FOR CHANGES                    
         L     R4,COMATWAL                                                      
         USING PKGL1H,R4                                                        
         MVC   LSVDPT,BDPT         SAVE DAYPART/LENGTH                          
         MVC   LSVSLN,BSLN                                                      
         GOTO1 AVALDAY,PKGDAYH     DAYS                                         
         BNE   VALSX                                                            
         GOTO1 AVALTIM,PKGTIMH     TIMES                                        
         BNE   VALSX                                                            
         L     R8,AIOAREA1         DEFAULT DPT IS PACKAGE MASTER DPT            
         MVC   BDPT,BWDDPT-BWDRECD(R8)                                          
         CLI   INREC,RECPKG        TEST PACKAGE                                 
         BNE   VALS10                                                           
         GOTO1 AVALDPL,PKGDPTH     YES-VALIDATE DAYPART                         
         BE    VALS10                                                           
         CLC   FVMSGNO,=AL2(FVFNONE)   TEST DAYPART MISSING                     
         BNE   VALSX                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
*                                                                               
VALS10   MVC   BSLN,BWDSLN-BWDRECD(R8) SLN IS PACKAGE MASTER SLN                
         XC    APFULL,APFULL       COST/USER CODE                               
         MVC   LUCODE,SPACES                                                    
         LA    R1,PKGCOSH                                                       
         CLI   INREC,RECPKG                                                     
         BE    *+8                                                              
         LA    R1,ORBCOSH                                                       
         GOTO1 AFVAL                                                            
         BH    VALSX                                                            
         BL    VALS14                                                           
         CLC   FVIFLD(2),=C'U='    TEST FOR USER CODE                           
         BNE   VALS12                                                           
         MVC   LUCODE,FVIFLD+2     YES                                          
         CLC   LUCODE,SPACES                                                    
         BE    ENOTV                                                            
         B     VALS14                                                           
*                                                                               
VALS12   ZIC   RE,FVILEN                                                        
         ST    RE,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   0(R1),0                                                          
         BNE   EICS                                                             
         GOTO1 ANETCOST,APPARM+4   NET DOWN THE COST IF NECESSARY               
         MVC   APFULL,APPARM+4                                                  
*                                                                               
VALS14   TM    LFLAG,LNEWLINE      TEST NEW LINE                                
         BO    VALS20                                                           
         CLC   BDAYS,BWDPODAY      NO-COMPARE DAYS/TIMES/DPT/COST               
         BE    *+8                                                              
         OI    LFLAG,LCHGREC                                                    
         CLC   BTIMES,BWDTIMES                                                  
         BE    *+8                                                              
         OI    LFLAG,LCHGREC                                                    
         CLI   INREC,RECPKG        TEST PACKAGE                                 
         BNE   VALS16                                                           
         CLC   BDPT,BWDDPT         YES-DAYPART                                  
         BE    VALS16                                                           
         OI    LFLAG,LCHGREC                                                    
*                                                                               
VALS16   CLC   LUCODE,SPACES       TEST USER CODE                               
         BNH   VALS17                                                           
         CLC   LUCODE,BWDUCODE     YES-TEST FOR CHANGE                          
         BE    VALS18                                                           
         OI    LFLAG,LCHGREC                                                    
         B     VALS18                                                           
*                                                                               
VALS17   CLC   BWDCOST1,APFULL                                                  
         BE    VALS18                                                           
         MVC   LOLDCOST,BWDCOST1   SAVE OLD COST                                
         OI    LFLAG,LCHGCST                                                    
*                                                                               
VALS18   TM    LFLAG,LCHG          TEST ANY CHANGES                             
         BZ    VALS40              NO                                           
*                                                                               
VALS20   GOTO1 AGETRTG             SAVE TARGET RATING                           
         MVC   LOLDRTG,LRATING                                                  
*                                                                               
         MVC   IOKEY(13),BWDKEY    READ WORK RECORDS TO MATCH THIS LINE         
         LA    R3,IOKEY                                                         
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,BDAYS      SET DAYS/TIMES                               
         MVC   BWDKELTM,PTIMES                                                  
         MVI   BWDKELSQ,0                                                       
         XC    LKEYSAVE,LKEYSAVE                                                
         XC    LCSTSAVE,LCSTSAVE                                                
         LA    R1,MINHI3                                                        
         B     VALS22+4                                                         
*                                                                               
VALS22   LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     *+14                                                             
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BE    VALS24                                                           
         OC    LKEYSAVE,LKEYSAVE   TEST MATCH                                   
         BNZ   VALS30                                                           
         OI    PKGDAYH+FVIIND-FVIHDR,FVIVAL  NO - MARK LINE VALIDATED           
         OI    PKGTIMH+FVIIND-FVIHDR,FVIVAL                                     
         LA    R1,PKGCOSH                                                       
         CLI   INREC,RECPKG                                                     
         BNE   *+12                                                             
         OI    PKGDPTH+FVIIND-FVIHDR,FVIVAL                                     
         B     *+8                                                              
         LA    R1,ORBCOSH                                                       
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         MVC   FVADDR,COMATWAL                                                  
         B     EWNF                               ERROR EXIT                    
         DROP  R4                                                               
*                                                                               
VALS24   L     R3,AIOAREA3                                                      
         CLC   BWDDPT,BDPT         MATCH DAYPART/LENGTH/TIMES                   
         BNE   VALS22                                                           
         CLC   BWDSLN,BSLN                                                      
         BNE   VALS22                                                           
         CLC   BWDTIMES,BTIMES                                                  
         BNE   VALS22                                                           
         CLI   FVILEN,0            MATCHED-TEST COST INPUT                      
         BNE   VALS26                                                           
         OC    LKEYSAVE,LKEYSAVE   NO-TEST ALREADY ONE MATCH                    
         BNZ   *+20                                                             
         MVC   LKEYSAVE,BWDKEY     NO-SAVE THIS RECORD KEY AND COST             
         MVC   LCSTSAVE,BWDCOST1                                                
         B     VALS22                                                           
         CLC   BWDCOST1,LCSTSAVE   YES-TEST SAME COST AS BEFORE                 
         BNE   ENONE               NO-COST IS MISSING INPUT FIELD               
         B     VALS22                                                           
*                                                                               
VALS26   CLC   LUCODE,SPACES       TEST USER CODE INPUT                         
         BNH   VALS28                                                           
         CLC   BWDUCODE,LUCODE     YES-TRY TO MATCH USER CODE                   
         BNE   VALS22                                                           
         MVC   LCSTSAVE,BWDCOST1   YES-SAVE THE COST                            
         B     VALS31                                                           
*                                                                               
VALS28   OC    LKEYSAVE,LKEYSAVE   COST WAS INPUT-TEST ALREADY MATCHED          
         BNZ   *+16                                                             
         MVC   LKEYSAVE,BWDKEY     NO-SAVE RECORD KEY AND COST                  
         MVC   LCSTSAVE,APFULL                                                  
         CLC   BWDCOST1,APFULL     TEST THIS RECORD HAS THE COST                
         BNE   VALS22              NO-CONTINUE READING                          
         B     VALS31              YES-USE THIS RECORD                          
*                                                                               
VALS30   MVC   IOKEY(13),LKEYSAVE  GET WORK RECORD                              
         GOTO1 AMIN,MINRD3                                                      
         BE    VALS31                                                           
         DC    H'0'                                                             
*                                                                               
VALS31   L     R3,AIOAREA2         BUILD PKG LINE                               
         TM    LFLAG,LNEWLINE      TEST NEW PKG LINE                            
         BO    VALS38                                                           
         SR    R0,R0               NO - SAVE SPOTS/WEEK AND TRANSFER            
         LA    R9,BWDEL                 ELEMENTS                                
         XC    LSPWEL,LSPWEL                                                    
         XC    LBTREL,LBTREL                                                    
*                                                                               
VALS32   CLI   0(R9),0                                                          
         BE    VALS38                                                           
         LA    R1,LSPWEL                                                        
         CLI   0(R9),SPWELCDQ                                                   
         BE    VALS34                                                           
         LA    R1,LBTREL                                                        
         CLI   0(R9),BTRELCDQ                                                   
         BNE   VALS36                                                           
*                                                                               
VALS34   ZIC   RE,1(R9)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     VALS36                                                           
         MVC   0(0,R1),0(R9)                                                    
*                                                                               
VALS36   IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     VALS32                                                           
*                                                                               
VALS38   XC    BWDEL(256),BWDEL                                                 
         MVC   APFULL,LCSTSAVE     COST                                         
         GOTO1 ABLDPKG             BUILD ELEMENTS FROM WORK RECORD              
*                                                                               
         TM    LFLAG,LNEWLINE      TEST NEW                                     
         BO    VALS40                                                           
         LA    R1,LSPWEL           NO - ADD BACK SAVED ELEMENTS                 
         BAS   RE,ADDELEM                                                       
         LA    R1,LBTREL                                                        
         BAS   RE,ADDELEM                                                       
         CLI   LSPWEL,0            TEST SPOTS/WEEK ELEMENT                      
         BE    VALS40                                                           
         GOTO1 AGETRTG             YES-GET TARGET RATING                        
         CLC   LRATING,LOLDRTG     TEST RATING CHANGE                           
         BE    VALS39                                                           
         GOTO1 AACTPTS             YES - CALCULATE NEW ACTUAL POINTS            
*                                                                               
VALS39   TM    LFLAG,LCHGCST       TEST COST CHANGE                             
         BZ    VALS40                                                           
         GOTO1 AACTDOL             YES - CALCULATE NEW ACTUAL DOLLARS           
*                                                                               
VALS40   MVC   BDPT,LSVDPT         RESTORE DAYPART/LENGTH                       
         MVC   BSLN,LSVSLN                                                      
         CLI   INREC,RECPKG        TEST PACKAGE                                 
         BNE   VALS42                                                           
         L     RF,COMATWAL         SEE IF ANY CHANGES ON CORRESPONDING          
         LA    RE,PKGL1H           SCHEDULE LINE                                
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         LA    R1,PKGL2H-PKGL1H                                                 
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STC   RF,LINE                                                          
         LA    R1,PKGDT2H-PKGDT1H                                               
         MR    RE,R1                                                            
         LA    R9,PKGDT1H                                                       
         AR    R9,RF                                                            
         USING PKGDT1H,R9                                                       
         TM    PKGSKDH+FVIIND-FVIHDR,FVIVAL   TEST SKED FIELD CHANGED           
         BO    VALS42                                                           
*                                  YES - VALIDATE SKED AND BUILD                
         GOTO1 AVALSKD,APPARM,(LINE,PKGSKDH),0   SPOTS/WEEK ELEMENT             
         BNE   VALSX                                                            
         OI    PKGSKDH+FVATRB-FVIHDR,FVAHIGH    HIGHLIGHT THE SKED LINE         
         OI    PKGDT1H+FVATRB-FVIHDR,FVAHIGH                                    
         TM    COMCHG,LSKED        TEST SCHEDULE CHANGE                         
         BZ    VALS42                                                           
         OI    TWAFLAG,TWAFMTPD    YES - INDICATE FORMAT ACTUAL PTS/DOL         
*                                                                               
VALS42   TM    LFLAG,LCHG+LNEWLINE   TEST NEW OR CHANGED RECORD                 
         BZ    VALS44                                                           
         L     R4,COMATWAL           YES - DISPLAY RECORD                       
         GOTO1 ADISPKG                                                          
         CLI   INREC,RECPKG          FOR PACKAGE RECORDS --                     
         BNE   VALS46                                                           
         LA    R8,PKGDT1             DISPLAY LHS OF SKED LINE                   
         USING SKDLINED,R8                                                      
         MVC   SLLINE,LDSPLINE                                                  
         MVC   SLDAYS,QDAYS                                                     
         MVC   SLTIMES,QTIMES                                                   
         OI    PKGDT1H+6,FVOXMT                                                 
         TM    LFLAG,LNEWLINE      TEST NEW LINE                                
         BZ    VALS46                                                           
         MVC   PKGSKD,SPACES       YES-FORMAT RHS OF SKED LINE                  
         LA    R8,PKGSKD+1                                                      
         ZIC   RE,CMPNWKS                                                       
         MVI   1(R8),C'.'                                                       
         LA    R8,4(R8)                                                         
         BCT   RE,*-8                                                           
         B     VALS46                                                           
         DROP  R9                                                               
*                                                                               
VALS44   CLI   INREC,RECPKG          TEST PACKAGE                               
         BNE   VALSX                                                            
         TM    COMCHG,LSKED          YES-TEST SCHEDULE CHANGE                   
         BZ    VALSX                                                            
*                                                                               
VALS46   OI    SVACTIV2,SVVALSEL   INDICATE ACTIVITY                            
         TM    TWAFLAG,TWAFMTPD    TEST NEW ACTUAL POINTS/DOLLARS               
         BZ    VALS47                                                           
*****    GOTO1 AFMACPTS,APPARM,PKGACTH,SVPKGVA  YES-REDISPLAY ACTUAL            
         LR    RE,R5                                                            
         AHI   RE,SVPKGVA-TWAD                                                  
         ST    RE,APPARM+4                                                      
         GOTO1 AFMACPTS,APPARM,PKGACTH,,0       YES-REDISPLAY ACTUAL            
*****    GOTO1 AFMACDOL,(R1),PKGACTH,SVPKGVA        POINTS AND DOLLARS          
         LR    RE,R5                                                            
         AHI   RE,SVPKGVA-TWAD                                                  
         ST    RE,APPARM+4                                                      
         GOTO1 AFMACDOL,(R1),PKGACTH,,0             POINTS AND DOLLARS          
*                                                                               
VALS47   TM    LFLAG,LNEWLINE      TEST NEW LINE                                
         BZ    VALS48                                                           
         GOTO1 AMIN,MINADD2        YES - ADD THE RECORD                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    APRECDA,APRECDA     SET VALUES FOR ROOT                          
         MVI   APRECID,0                                                        
         MVC   APRECNUM,INREC                                                   
         MVC   APRECKEY(13),BWDKEY                                              
         B     VALSX                                                            
*                                                                               
VALS48   MVC   IOKEY(13),APRECKEY  NO - PUT THE RECORD                          
         GOTO1 AMIN,MINRD3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AMIN,MINWRT2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APRECKEY(13),BWDKEY      SAVE KEY FOR GENERAL                    
*                                                                               
VALSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT                                           *         
* R1 = A(ELEMENT)                                                     *         
* R3 = A(RECORD)                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
ADDELEM  LR    R0,RE                                                            
         CLI   0(R1),0                                                          
         BER   RE                                                               
         ZIC   RF,1(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)                                                  
         GOTO1 AADDELS,BWDRECD                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EPNF     MVC   FVMSGNO,=AL2(FVNOPKOR)                                           
         B     EXIT                                                             
EICS     MVC   FVMSGNO,=AL2(FVICST)                                             
         B     EXIT                                                             
EDNF     MVC   FVMSGNO,=AL2(FVIDEMES)                                           
         B     EXIT                                                             
EWNF     MVC   FVMSGNO,=AL2(FVIWKREC)                                           
         B     EXIT                                                             
EDUP     MVC   FVMSGNO,=AL2(FVDUPDEM)                                           
         B     EXIT                                                             
E53WKS   MVC   FVMSGNO,=AL2(FVPTL)  PERIOD CANNOT BE LONGER THAN 14 WKS         
         B     EXIT                                                             
E53DYS   MVC   FVMSGNO,=AL2(FVPTL2) PERIOD CANNOT BE LONGER THAN 2 WKS          
         B     EXIT                                                             
ENWR     MVC   FVMSGNO,=AL2(FVNOWKRC)                                           
         B     EXIT                                                             
ENOTV    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
ENONE    MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                                                             
EDLYORB  MVC   FVMSGNO,=AL2(FVIDLYOR)                                           
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
FF       EQU   X'FF'                                                            
NPKGLINS EQU   5                                                                
NORBLINS EQU   9                                                                
XFF      DC    XL4'FFFFFFFF'                                                    
PFKLINE  DC    C'Enter=Next PF12=Quit'                                          
SPACES   DC    64C' '                                                           
         EJECT                                                                  
***********************************************************************         
* EDIT A DEMO VALUE                                                   *         
* IN  : R9 = A(OUTPUT AREA)                                           *         
*       R1 = A(DEMO ELEMENT ENTRY)                                    *         
* OUT : R9 = A(POSITION TO RIGHT OF EDITED VALUE)                     *         
***********************************************************************         
         SPACE 1                                                                
VPEDITDM NTR1  BASE=*,LABEL=*                                                   
         ST    R9,EBAOUT                                                        
         MVC   APFULL,4(R1)                                                     
         NI    APFULL,FF-DMODEMOV                                               
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
***  2 DECIMAL                                                                  
         TM    APFULL,DMODEM2D     IS IT 2 DECIMALS?                            
         BO    VPEDIT30             - YUP                                       
***  2 DECIMAL                                                                  
         CLC   APFULL,=F'10000'                                                 
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         B     VPEDIT50                                                         
*                                                                               
***  2 DECIMAL                                                                  
VPEDIT30 NI    APFULL,FF-DMODEM2D   TAKE OFF 2 DECIMAL FLAG                     
         MVI   EBDECS,2                                                         
         CLC   APFULL,=F'100000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
***  2 DECIMAL                                                                  
*                                                                               
VPEDIT50 OI    EBOPT,X'20'         ZERO=NOBLANK                                 
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R0,5                                                             
         CLI   0(R9),C' '                                                       
         BNH   *+12                                                             
         LA    R9,1(R9)                                                         
         BCT   R0,*-12                                                          
         XIT1  REGS=(R9)                                                        
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**BW8X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     DISPKG                                                           
         B     GETGVA                                                           
         B     BLDPKG                                                           
         B     VALDATES                                                         
         B     GETRTG                                                           
         B     ACTPTS                                                           
         B     ACTDOL                                                           
*                                                                               
XIT      CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY A PACKAGE LINE                                   *         
* INPUT  : R4 = A(TWA LINE)                                           *         
*          IOAREA2 CONTAINS THE RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
DISPKG   DS    0H                                                               
         L     RC,APALOCAL                                                      
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         USING PKGL1H,R4                                                        
         ZIC   RE,BWDKELSQ         DISPLAY LINE NUMBER                          
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PKGLIN,APDUB                                                     
         MVI   PKGLIN,C' '                                                      
         MVC   LDSPLINE,PKGLIN+1   SAVE LINE NUM IN DISPLAY FORMAT              
         OI    PKGLINH+6,FVOXMT                                                 
         MVC   LSVDPT,BDPT         SAVE DAYPART/LENGTH                          
         MVC   LSVSLN,BSLN                                                      
         GOTO1 AGETDAY,BWDPODAY    DAYS                                         
         MVC   PKGDAY,QDAYS                                                     
         OI    PKGDAYH+6,FVOXMT                                                 
         GOTO1 AGETTIM,BWDTIMES    TIMES                                        
         MVC   PKGTIM,QTIMES                                                    
         OI    PKGTIMH+6,FVOXMT                                                 
         MVC   BDPT,LSVDPT         RESTORE DAYPART/LENGTH                       
         MVC   BSLN,LSVSLN                                                      
         LA    R8,ORBCOSH                                                       
         CLI   INREC,RECPKG        TEST PACKAGE RECORD                          
         BNE   DISP2                                                            
         MVC   PKGDPT,BWDDPT       YES-FORMAT DAYPART                           
         OI    PKGDPTH+6,FVOXMT                                                 
         LA    R8,PKGCOSH                                                       
*                                                                               
DISP2    OC    BWDCOST1,BWDCOST1   COST                                         
         BNZ   *+20                                                             
         MVC   8(L'PKGCOS,R8),BLANKS                                            
         MVC   8+4(2,R8),=C'$0'                                                 
         B     DISP4                                                            
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
         LA    R1,BWDCOST1                                                      
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         LA    R1,8(R8)                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'PKGCOS                                                  
         CLC   BWDCOST1,=F'10000'                                               
         BL    *+12                                                             
         MVI   EBSCIN,X'82'                                                     
         MVI   EBDECS,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISP4    OI    6(R8),FVOXMT                                                     
*                                                                               
         LA    R8,PKGPRDH                                                       
         LA    RE,L'PKGPRD-1                                                    
         LA    RF,L'PLPPROG-1                                                   
         LA    R1,L'PLOPROG-1                                                   
         CLI   INREC,RECPKG                                                     
         BE    *+16                                                             
         LA    R8,ORBPRDH                                                       
         LA    RE,L'ORBPRD-1                                                    
         LA    RF,L'PLOPROG-1                                                   
         OI    6(R8),FVOXMT                                                     
         LA    R8,8(R8)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R8),0(R8)                                                    
         USING PKGLINED,R8                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLPROG(0),BWDPROG   PROGRAMMING                                  
         CLI   INREC,RECPKG                                                     
         BNE   *+12                                                             
         SR    R1,RF                                                            
         BCTR  R8,0                                                             
         BCT   R1,*-2                                                           
*                                                                               
         SR    R0,R0                                                            
         LA    R9,BWDEL            DEMOS                                        
         XC    LADEMEL,LADEMEL                                                  
         XC    LABTREL,LABTREL                                                  
*                                                                               
DISP6    CLI   0(R9),0                                                          
         BE    DISP10                                                           
         LA    R1,LADEMEL                                                       
         CLI   0(R9),DMOELCDQ      TEST DEMO ELEMENT                            
         BE    DISP8                                                            
         LA    R1,LABTREL                                                       
         CLI   0(R9),BTRELCDQ      TEST TRANSFER ELEMENT                        
         BNE   DISP8+4                                                          
*                                                                               
DISP8    ST    R9,0(R1)            YES - SAVE ITS ADDRESS                       
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     DISP6                                                            
*                                                                               
DISP10   ICM   R9,15,LADEMEL                                                    
         BZ    DISP20                                                           
         USING DMOEL,R9                                                         
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R9)                                                         
         AR    RF,R9                                                            
         BCTR  RF,0                                                             
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         LA    R1,APFULL                                                        
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'PLDEM1                                                  
         LA    R0,4                                                             
         LA    R2,ESTDEMS                                                       
         LA    R6,PLDEM1                                                        
*                                                                               
DISP12   OC    0(3,R2),0(R2)                                                    
         BZ    DISP20                                                           
         LA    R1,DMODEMO                                                       
         CLC   1(2,R1),1(R2)                                                    
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DISP18                                                           
         MVC   APFULL,4(R1)                                                     
         MVI   EBFLOAT,0                                                        
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         ST    R6,EBAOUT                                                        
         TM    APFULL,DMODEMOV                                                  
         BZ    *+12                                                             
         MVI   EBFLOAT,C'*'                                                     
         NI    APFULL,FF-DMODEMOV                                               
***  2 DECIMAL                                                                  
         TM    APFULL,DMODEM2D     WE DOING 2 DECIMAL?                          
         BO    DISP15C                                                          
***  2 DECIMAL                                                                  
         CLC   APFULL,=F'100000'                                                
         BNL   DISP14                                                           
         CLI   EBFLOAT,C'*'                                                     
         BNE   DISP16                                                           
         CLC   APFULL,=F'10000'                                                 
         BL    DISP16                                                           
*                                                                               
DISP14   MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         B     DISP16                                                           
*                                                                               
***  2 DECIMAL                                                                  
DISP15C  DS    0H                                                               
         MVI   EBDECS,2                                                         
         NI    APFULL,FF-DMODEM2D                                               
         CLC   APFULL,=F'1000000'                                               
         BNL   DISP15G                                                          
         CLI   EBFLOAT,C'*'                                                     
         BNE   DISP16                                                           
         CLC   APFULL,=F'100000'                                                
         BL    DISP16                                                           
*                                                                               
DISP15G  MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'        SCALE 100X BACK                              
***  2 DECIMAL                                                                  
*                                                                               
DISP16   STM   RE,RF,APDUB                                                      
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LM    RE,RF,APDUB                                                      
*                                                                               
DISP18   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R6,PLDEM2-PLDEM1(R6)                                             
         BCT   R0,DISP12                                                        
*                                                                               
DISP20   TM    BWDINDS,BWDITRLK    TEST BUY TRANSFER LOCK-OUT                   
         BZ    *+12                                                             
         MVI   PKGLIN,C':'         YES - MARK LINE WITH COLON                   
         B     DISPX                                                            
         OC    LABTREL,LABTREL     TEST BUY TRANSFER ELEMENT                    
         BZ    DISPX                                                            
         MVI   PKGLIN,C'*'         YES - MARK LINE WITH *                       
*                                                                               
DISPX    B     XIT                                                              
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AND DISPLAY GOAL VS ACTUAL DOLLAR AND POINTS INFO    *         
***********************************************************************         
         SPACE 1                                                                
GETGVA   DS    0H                                                               
         L     RC,APALOCAL                                                      
         USING BWDRECD,R3                                                       
*****    XC    SVPKAPTS(NMAXWKS*4),SVPKAPTS   CLEAR ACTUAL PTS/DOL              
         LR    RE,R5                                                            
         AHI   RE,SVPKAPTS-TWAD                                                 
         XC    0(53*4,RE),0(RE)      CLEAR ACTUAL PTS/DOL                       
*****    XC    SVPKADOL,SVPKADOL                                                
         LR    RE,R5                                                            
         AHI   RE,SVPKADOL-TWAD                                                 
         XC    0(L'SVPKADOL,RE),0(RE)      CLEAR ACTUAL PTS/DOL                 
*                                                                               
         XC    LDPTS,LDPTS                                                      
         MVC   IOKEY(13),APRECKEY                                               
         LA    R3,IOKEY            READ RECORDS TO ACCUMULATE ACTUAL            
         MVI   BWDKTYP,BWDKTYPQ    MAKE SURE 1ST BYTE IS NOT X'FF'!             
         MVC   BWDKELPO,LPKGNUM    POINTS AND DOLLARS                           
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         MVI   BWDKELSQ,1                                                       
         LA    R1,MINHI2                                                        
         B     GVA2+4                                                           
*                                                                               
GVA2     LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GVA22                                                            
         CLC   IOKEY(BWDKELDY-BWDKEY),IOKEYSAV                                  
         BNE   GVA22                                                            
         L     R3,AIOAREA2                                                      
         LA    R1,LDPTS            ADD DAYPART TO LIST                          
         LA    R0,L'LDPTS                                                       
*                                                                               
GVA4     CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),BWDDPT                                                   
         B     GVA6                                                             
         CLC   BWDDPT,0(R1)                                                     
         BE    GVA6                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,GVA4                                                          
         DC    H'0'                                                             
*                                                                               
GVA6     LA    R4,BWDEL                                                         
         SR    R0,R0               FIND SPOTS PER WEEK AND DEMO ELEMS           
         XC    APDUB,APDUB                                                      
*                                                                               
GVA8     CLI   0(R4),0                                                          
         BE    GVA10               NOT FOUND                                    
         LA    R1,APDUB            APDUB=A(SPOTS PER WEEK ELEM)                 
         CLI   0(R4),SPWELCDQ                                                   
         BE    *+16                                                             
         LA    R1,APDUB+4          APDUB+4=A(DEMO ELEM)                         
         CLI   0(R4),DMOELCDQ                                                   
         BNE   *+8                                                              
         ST    R4,0(R1)                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GVA8                                                             
*                                                                               
GVA10    OC    APDUB(4),APDUB      TEST SPOTS PER WEEK ELEM                     
         BZ    GVA2                                                             
         ICM   R4,15,APDUB+4       YES-GET TARGET RATING                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DMODEMO-DMOEL(R4)                                             
         OC    INORTG,INORTG                                                    
         BZ    GVA12                                                            
         MVI   APWORK,C'R'         OVERRIDE RATING                              
         MVC   APWORK+1(1),INORTG+2                                             
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         CLC   1(2,R1),APWORK                                                   
         BE    GVA12                                                            
         BXLE  R1,RE,*-10                                                       
         B     GVA18                                                            
*                                                                               
GVA12    MVC   LRATING,4(R1)                                                    
         L     R4,APDUB                                                         
         USING SPWEL,R4                                                         
         ZIC   R0,SPWELLN                                                       
         SH    R0,=Y(SPWPERWK-SPWEL)                                            
         ZIC   RE,CMPNWKS                                                       
         CR    R0,RE                                                            
         BNH   *+6                                                              
         LR    R0,RE                                                            
         ST    R0,APFULL           R0=N'WEEKS                                   
****  THE FOLLOWING LINE SHOULD HAVE TAKEN CARE OF 2 DECIMAL FLAG               
         MVI   LRATING,0                                                        
         ICM   R2,15,LRATING       TEST RATING=0                                
         BZ    GVA18               YES                                          
         LA    R8,SPWPERWK         NO-ACCUMULATE TOTAL RATINGS BY WEEK          
         LR    R9,R5                                                            
         AHI   R9,SVPKAPTS-TWAD                                                 
*                                                                               
GVA14    SR    RF,RF                                                            
         ICM   RF,1,0(R8)                                                       
         BZ    GVA16                                                            
         MR    RE,R2               SPOTS X RATING                               
***  2 DECIMAL                                                                  
         TM    4(R1),X'40'         IS THIS NUMBER 2 DECIMAL?                    
         BZ    GVA15G               - NOPE, IT'S NOT                            
         D     RE,=F'10'                                                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
***  2 DECIMAL                                                                  
GVA15G   L     RE,0(R9)                                                         
         AR    RE,RF                                                            
         ST    RE,0(R9)                                                         
*                                                                               
GVA16    LA    R8,1(R8)                                                         
         LA    R9,4(R9)                                                         
         BCT   R0,GVA14                                                         
*                                  ACCUMULATE TOTAL COST                        
GVA18    ICM   R2,15,BWDCOST1      R2=COST                                      
         BZ    GVA2                                                             
         L     R0,APFULL                                                        
         LA    R8,SPWPERWK                                                      
*****                                                                           
         LR    R1,R5               R1=COST ACCUMULATOR                          
         AHI   R1,SVPKADOL-TWAD                                                 
         L     R1,0(R1)                                                         
*                                                                               
GVA20    SR    RF,RF                                                            
         ICM   RF,1,0(R8)                                                       
         BZ    *+8                                                              
         MR    RE,R2               SPOTS X COST                                 
         AR    R1,RF                                                            
         LA    R8,1(R8)            NEXT WEEK                                    
         BCT   R0,GVA20                                                         
*****    ST    R1,SVPKADOL         SAVE TOTAL COST SO FAR                       
         LR    RF,R5               R1=COST ACCUMULATOR                          
         AHI   RF,SVPKADOL-TWAD                                                 
         ST    R1,0(RF)                                                         
         B     GVA2                READ NEXT PACKAGE SLAVE                      
*                                                                               
GVA22    DS    0H                                                               
******   XC    SVPKGDOL,SVPKGDOL   GET GOALS FOR ALL DAYPARTS                   
         LR    RF,R5               GET GOALS FOR ALL DAYPARTS                   
         AHI   RF,SVPKGDOL-TWAD                                                 
         XC    0(L'SVPKGDOL,RF),0(RF)                                           
*                                                                               
*****    XC    SVPKGPTT,SVPKGPTT                                                
         LR    RF,R5                                                            
         AHI   RF,SVPKGPTT-TWAD                                                 
         XC    0(L'SVPKGPTT,RF),0(RF)                                           
*                                                                               
*****    XC    SVPKGPTS(NMAXWKS*4),SVPKGPTS                                     
         LR    RF,R5                                                            
         AHI   RF,SVPKGPTS-TWAD                                                 
         XC    0(53*4,RF),0(RF)                                                 
*                                                                               
         MVC   LSVDPT,BDPT         SAVE DAYPART/LENGTH                          
         MVC   LSVSLN,BSLN                                                      
         L     R1,AIOAREA1                                                      
         MVC   BSLN,BWDSLN-BWDRECD(R1)                                          
         LA    R4,LDPTS                                                         
         LA    R8,L'LDPTS                                                       
*                                                                               
GVA24    CLI   0(R4),0                                                          
         BE    GVA28                                                            
         MVC   BDPT,0(R4)                                                       
         XC    DPTSUBS,DPTSUBS    DO NOT INCLUDE SUB-DAYPARTS (FOR NOW)         
         GOTO1 AGETGOAL                                                         
         L     R1,AIOAREA3                                                      
*****    L     RE,SVPKGDOL                                                      
         LR    RF,R5                                                            
         AHI   RF,SVPKGDOL-TWAD                                                 
         L     RE,0(RF)                                                         
         A     RE,0(R1)                                                         
*****    ST    RE,SVPKGDOL                                                      
         ST    RE,0(RF)                                                         
*                                                                               
*****    L     RE,SVPKGPTT                                                      
         LR    RF,R5                                                            
         AHI   RF,SVPKGPTT-TWAD                                                 
         L     RE,0(RF)                                                         
         A     RE,4(R1)                                                         
*****    ST    RE,SVPKGPTT                                                      
         ST    RE,0(RF)                                                         
*                                                                               
         LA    R1,64(R1)                                                        
*****    LA    RF,SVPKGPTS                                                      
         LR    RF,R5                                                            
         AHI   RF,SVPKGPTS-TWAD                                                 
         LA    R0,NMAXWKS                                                       
*                                                                               
GVA26    L     RE,0(RF)                                                         
         A     RE,0(R1)                                                         
         ST    RE,0(RF)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,GVA26                                                         
*                                                                               
         LA    R4,1(R4)            NEXT DAYPART                                 
         BCT   R8,GVA24                                                         
*                                                                               
GVA28    DS    0H                                                               
         GOTO1 AGETDPT,LSVDPT      GET BACK ORIGINAL DAYPART                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDPT,LSVDPT         RESTORE DAYPART/LENGTH                       
         MVC   BSLN,LSVSLN                                                      
         MVI   TWADPT,0                                                         
         MVI   TWASLN,0                                                         
*****    GOTO1 AFMTGOAL,APPARM,PKGGOLH,PKGCMTH,SVPKGVA  FORMAT GOALS            
         LR    RE,R5                                                            
         AHI   RE,SVPKGVA-TWAD                                                  
         ST    RE,APPARM+8                                                      
         GOTO1 AFMTGOAL,APPARM,PKGGOLH,PKGCMTH,,0       FORMAT GOALS            
*                                                                               
*****    GOTO1 AFMACPTS,(R1),PKGACTH,SVPKGVA         AND ACTUAL PTS/DOL         
         LR    RE,R5                                                            
         AHI   RE,SVPKGVA-TWAD                                                  
         ST    RE,APPARM+4                                                      
         GOTO1 AFMACPTS,(R1),PKGACTH,,0              AND ACTUAL PTS/DOL         
*                                                                               
*****    GOTO1 AFMACDOL,(R1),PKGACTH,SVPKGVA                                    
         LR    RE,R5                                                            
         AHI   RE,SVPKGVA-TWAD                                                  
         ST    RE,APPARM+4                                                      
         GOTO1 AFMACDOL,(R1),PKGACTH,,0                                         
*                                                                               
GVAX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PACKAGE LINE ELEMENTS FROM WORK RECORD             *         
* INPUT  : R3 = A(PACKAGE RECORD)                                     *         
*          IOAREA3 CONTAINS WORK RECORD                               *         
*          APFULL = COST                                              *         
***********************************************************************         
         SPACE 1                                                                
BLDPKG   DS    0H                                                               
         USING BWDRECD,R3                                                       
         L     R1,AIOAREA3         MOVE DESCRIPTION ELEMENT                     
         LA    R1,BWDEL-BWDRECD(R1)                                             
         ZIC   RE,1(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BWDEL(0),0(R1)                                                   
         MVC   BWDPKOR,BWDKELPO    PACKAGE/ORBIT NUMBER                         
         MVC   BWDPODAY,BWDDAYS-BWDEL(R1)                                       
         MVI   BWDDAYS,0                                                        
         XC    BWDTIMCD,BWDTIMCD                                                
         MVC   BWDSEQ,BWDKELSQ     PACKAGE/ORBIT SLAVE SEQUENCE NUMBER          
         MVC   BWDCOST1,APFULL     COST                                         
         XC    BWDEFDT2,BWDEFDT2   CLEAR EFFECTIVE DATES/COSTS                  
         XC    BWDCOST2,BWDCOST2                                                
         XC    BWDEFDT3,BWDEFDT3                                                
         XC    BWDCOST3,BWDCOST3                                                
         NI    BWDINDS,FF-BWDITRLK     NO BUY TRANSFER LOCK-OUT                 
         CLI   INREC,RECPKG        PACKAGE/ORBIT INDICATOR                      
         BNE   *+12                                                             
         OI    BWDINDS,BWDIPKG                                                  
         B     *+8                                                              
         OI    BWDINDS,BWDIORB                                                  
         XC    BWDTRED2,BWDTRED2   CLEAR BUY TRANSFER FIELDS                    
         XC    BWDTREC2,BWDTREC2                                                
         XC    BWDTRED3,BWDTRED3                                                
         XC    BWDTREC3,BWDTREC3                                                
*                                                                               
         SR    R0,R0               COPY SELECTED ELEMENTS                       
         L     R9,AIOAREA3                                                      
         LA    R9,BWDEL-BWDRECD(R9)                                             
*                                                                               
BLDP2    CLI   0(R9),0                                                          
         BE    BLDPX                                                            
         CLI   0(R9),DMOELCDQ      DEMO ELEMENT                                 
         BE    BLDP4                                                            
         CLI   0(R9),SHPELCDQ      SHARE/PUT OVERRIDE ELEMENT                   
         BE    BLDP4                                                            
         CLI   0(R9),UPGELCDQ      UPGRADE ELEMENT                              
         BE    BLDP4                                                            
         CLI   0(R9),ODTELCDQ      OVERRIDE DAY/TIME/STATION ELEMENT            
         BE    BLDP4                                                            
         CLI   0(R9),COMELCDQ      COMMENT ELEMENT                              
         BNE   BLDP6                                                            
*                                                                               
BLDP4    ZIC   RE,1(R9)            ADD THE ELEMENT TO PACKAGE LINE              
         BCTR  RE,0                                                             
         XC    APELEM,APELEM                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R9)                                                  
         GOTO1 AADDELS,BWDRECD                                                  
*                                                                               
BLDP6    IC    R0,1(R9)            NEXT ELEMENT                                 
         AR    R9,R0                                                            
         B     BLDP2                                                            
*                                                                               
BLDPX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATES FIELD                                                *         
* INPUT  : R1=A(DATES FIELD HEADER)                                   *         
* OUTPUT : APFULL=PACKED DATES START/END                              *         
*          APHALF=INACTIVE WEEKS MASK                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDATES XC    APFULL,APFULL                                                    
         XC    APHALF,APHALF                                                    
         GOTO1 AVALDAT                                                          
         BNE   VALDX                                                            
         MVI   FVINDX,0                                                         
         OC    APWORK+6(6),APWORK+6  CHECK FOR SECOND DATE                      
         BZ    VALD9                                                            
         CLC   APWORK(6),APWORK+6  CHECK START LE END                           
         BH    VALD8                                                            
         GOTO1 VDATCON,APPARM,(3,CMPST),APDUB  CHECK DATES WITHIN               
         CLC   APWORK(6),APDUB                 CAMPAIGN PERIOD                  
         BL    VALD7                                                            
         GOTO1 (RF),(R1),(3,CMPND),APDUB                                        
         CLC   APWORK+6(6),APDUB                                                
         BH    VALD7                                                            
         GOTO1 VDATCON,APPARM,APWORK,(2,APFULL)                                 
         GOTO1 (RF),(R1),APWORK+6,(2,APFULL+2)                                  
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP-TWAD                                                 
         LA    R0,NMAXWKS          SET INACTIVE WEEKS MASK                      
         SR    RE,RE                                                            
         LA    R1,1                                                             
         SLL   R1,31                                                            
         MVI   APBYTE,0                                                         
*                                                                               
VALD2    CLI   APBYTE,2            TEST BEYOND DATES                            
         BE    VALD4                                                            
         CLI   0(R4),X'FF'         TEST END OF FLIGHT WEEKS                     
         BNE   *+12                                                             
         MVI   APBYTE,2            YES-FORCE INACTIVE                           
         B     VALD4                                                            
         CLI   APBYTE,0            TEST BEFORE DATES                            
         BNE   VALD3                                                            
         CLC   APFULL(2),2(R4)     YES-TEST START DATE AFTER THIS WEEK          
         BH    VALD4                   YES-STILL BEFORE DATES                   
         MVI   APBYTE,1                NO-DURING DATES                          
         B     VALD5                      ACTIVE WEEK                           
*                                                                               
VALD3    CLC   APFULL+2(2),0(R4)    DURING DATES-TEST END DATE BEFORE           
         BNL   VALD5                             THIS WEEK                      
         MVI   APBYTE,2             YES-BEYOND DATES NOW                        
*                                                                               
VALD4    LR    RF,R1               INACTIVE WEEK                                
         B     VALD5+2                                                          
VALD5    SR    RF,RF               ACTIVE WEEK                                  
         SLDL  RE,1                                                             
         LA    R4,4(R4)                                                         
         BCT   R0,VALD2                                                         
*                                                                               
         SLL   RE,2                ASSUMES NMAXWKS=14                           
         STH   RE,APHALF                                                        
         B     VALDX                                                            
*                                                                               
VALD7    MVC   FVMSGNO,=AL2(FVIDTCAM) DATES NOT WITHIN CAMPAIGN PERIOD          
         B     VALDX                                                            
*                                                                               
VALD8    MVC   FVMSGNO,=AL2(FVIDAT)   INVALID DATE FORMAT                       
         B     VALDX                                                            
*                                                                               
VALD9    MVC   FVMSGNO,=AL2(FVNOEDT)  END DATE MISSING                          
*                                                                               
VALDX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE TARGET RATING                                    *         
* INPUT  : R3 = A(RECORD)                                             *         
* OUTPUT : LRATING = TARGET RATING                                    *         
***********************************************************************         
         SPACE 1                                                                
GETRTG   DS    0H                                                               
         USING BWDRECD,R3                                                       
         XC    LRATING,LRATING                                                  
         SR    R0,R0                                                            
         LA    R4,BWDEL            FIND DEMO ELEMENT                            
*                                                                               
GETR2    CLI   0(R4),0                                                          
         BE    GETRX                                                            
         CLI   0(R4),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETR2                                                            
         USING DMOEL,R4                                                         
         LA    R1,DMODEMO                                                       
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BZ    GETR4               NO - THEN TARGET = PRIMARY                   
         LA    RE,L'DMODEMO        YES - FIND TARGET                            
         ZIC   RF,DMOELLN                                                       
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         CLC   1(2,R1),INORTG+1                                                 
         BE    GETR4                                                            
         BXLE  R1,RE,*-10                                                       
         B     GETRX                                                            
*                                                                               
GETR4    MVC   LRATING,4(R1)                                                    
         NI    LRATING,FF-DMODEMOV                                              
***  2 DECIMAL                                                                  
         NI    LRATING,FF-DMODEM2D   TURN IT OFF                                
***  2 DECIMAL                                                                  
*                                                                               
GETRX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE ACTUAL POINTS                                     *         
* INPUT  : LOLDRTG = OLD TARGET RATING                                *         
*          LRATING = NEW TARGET RATING                                *         
*          R3 = A(RECORD)                                             *         
* OUTPUT : SVACPTS AND SVPKAPTS UPDATED                               *         
***********************************************************************         
         SPACE 1                                                                
ACTPTS   DS    0H                                                               
         USING BWDRECD,R3                                                       
         L     RE,LRATING                                                       
         S     RE,LOLDRTG          RE = RATING DIFFERENCE                       
         LA    R4,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
ACTP2    CLI   0(R4),0                                                          
         BE    ACTPX                                                            
         CLI   0(R4),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ACTP2                                                            
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         LA    R4,SPWPERWK-SPWEL(R4)                                            
*****    LA    R2,SVPKAPTS                                                      
         LR    R2,R5                                                            
         AHI   R2,SVPKAPTS-TWAD                                                 
         LA    R8,SVACPTS                                                       
         ZIC   R9,CMPNWKS                                                       
*                                                                               
ACTP4    ZIC   R1,0(R4)                                                         
         SR    R0,R0                                                            
         MR    R0,RE               SPOTS X CHANGE IN RATING                     
         L     R0,0(R2)                                                         
         AR    R0,R1                                                            
         ST    R0,0(R2)                                                         
         L     R0,0(R8)                                                         
         AR    R0,R1                                                            
         ST    R0,0(R8)                                                         
         LA    R4,1(R4)                                                         
         CR    R4,RF                                                            
         BNL   *+12                                                             
         LA    R8,4(R8)                                                         
         BCT   R9,ACTP4                                                         
         OI    TWAFLAG,TWAFMTPD    INDICATE FORMAT ACTUAL PTS/DOL               
*                                                                               
ACTPX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE ACTUAL DOLLARS                                    *         
* INPUT  : LOLDCOST = OLD COST                                        *         
*          R3 = A(RECORD)                                             *         
* OUTPUT : SVACDOL AND SVPKADOL UPDATED                               *         
***********************************************************************         
         SPACE 1                                                                
ACTDOL   DS    0H                                                               
         USING BWDRECD,R3                                                       
         ICM   RE,15,BWDCOST1                                                   
         S     RE,LOLDCOST         RE = COST DIFFERENCE                         
         LA    R4,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
ACTD2    CLI   0(R4),0                                                          
         BE    ACTDX                                                            
         CLI   0(R4),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ACTD2                                                            
         ZIC   R8,1(R4)                                                         
         AR    R8,R4                                                            
         LA    R4,SPWPERWK-SPWEL(R4)                                            
         ZIC   R9,CMPNWKS                                                       
*****    L     R2,SVPKADOL                                                      
         LR    RF,R5                                                            
         AHI   RF,SVPKADOL-TWAD                                                 
         L     R2,0(RF)                                                         
         L     RF,SVACDOL                                                       
*                                                                               
ACTD4    SR    R0,R0                                                            
         ZIC   R1,0(R4)                                                         
         MR    R0,RE                                                            
         AR    R2,R1                                                            
         AR    RF,R1                                                            
         LA    R4,1(R4)                                                         
         CR    R4,R8                                                            
         BNL   *+8                                                              
         BCT   R9,ACTD4                                                         
*                                                                               
         ST    RF,SVACDOL                                                       
*****    ST    R2,SVPKADOL                                                      
         LR    RF,R5                                                            
         AHI   RF,SVPKADOL-TWAD                                                 
         ST    R2,0(RF)                                                         
         OI    TWAFLAG,TWAFMTPD                                                 
*                                                                               
ACTDX    B     XIT                                                              
         EJECT                                                                  
BLANKS   DC    80C' '                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PKGLINED DSECT                                                                  
*                                                                               
PLPROG   DS    CL17                                                             
PLOPROG  DS    0CL17                                                            
PLPPROG  DS    0CL15                                                            
         DS    X                                                                
PLDEM1   DS    CL6                                                              
         DS    X                                                                
PLDEM2   DS    CL6                                                              
         DS    X                                                                
PLDEM3   DS    CL6                                                              
         DS    X                                                                
PLDEM4   DS    CL6                                                              
         SPACE 2                                                                
SKDLINED DSECT                                                                  
*                                                                               
SLLINE   DS    CL2                                                              
         DS    X                                                                
SLDAYS   DS    CL7                                                              
         DS    X                                                                
SLTIMES  DS    CL11                                                             
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS05/BWS08                   
*                                                                               
       ++INCLUDE SPNWS05WRK                                                     
         EJECT                                                                  
LOCALD   DSECT                                                                  
         ORG   LOCALD+2048                                                      
* BWS05 WORK AREA                                                               
         DS    0C                                                               
         SPACE                                                                  
         ORG   LOCALD+3072         ** BWS08 WORK AREA **                        
AXTRA    DS    0F                                                               
ADISPKG  DS    A                                                                
AGETGVA  DS    A                                                                
ABLDPKG  DS    A                                                                
AVALDATE DS    A                                                                
AGETRTG  DS    A                                                                
AACTPTS  DS    A                                                                
AACTDOL  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
LADEMEL  DS    F                                                                
LACMTEL  DS    F                                                                
LABTREL  DS    F                                                                
LSVGLPTS DS    (NMAXWKS)F                                                       
LOLDCOST DS    XL4                                                              
LPRIMRTG DS    XL4                                                              
LOLDRTG  DS    XL4                                                              
LRATING  DS    XL4                                                              
LUCODE   DS    XL4                                                              
LRECTAB  DS    XL(8*NORBLINS)                                                   
LPKGNUM  DS    XL1                                                              
LINE     DS    XL1                                                              
LASTLINE DS    XL1                                                              
LNLINES  DS    XL1                                                              
LSVDPT   DS    XL1                                                              
LSVSLN   DS    XL1                                                              
LDSPLINE DS    XL2                                                              
LDEMS    DS    XL12                                                             
LSPWEL   DS    XL32                                                             
LBTREL   DS    XL32                                                             
LKEYSAVE DS    XL13                                                             
LCSTSAVE DS    XL4                                                              
LDPTS    DS    CL16                                                             
*                                                                               
LFLAG    DS    X                                                                
LCHGREC  EQU   X'80'                                                            
LCHGCST  EQU   X'40'                                                            
LCHGDAT  EQU   X'20'                                                            
LCHG     EQU   LCHGREC+LCHGCST+LCHGDAT                                          
LDATES   EQU   X'08'                                                            
LNEWPKG  EQU   X'04'                                                            
LFIRST   EQU   X'02'                                                            
LNEWLINE EQU   X'01'                                                            
         SPACE                                                                  
         ORG   LOCALD+4096                                                      
         SPACE                                                                  
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSF4D                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSF2D                                                       
         EJECT                                                                  
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091SPNWS08   02/26/07'                                      
         END                                                                    
