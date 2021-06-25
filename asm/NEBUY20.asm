*          DATA SET NEBUY20    AT LEVEL 007 AS OF 12/13/10                      
*          DATA SET NEBUY20    AT LEVEL 001 AS OF 08/23/99                      
*          DATA SET NEBUY20    AT LEVEL 008 AS OF 04/08/98                      
*PHASE T31120A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - INFO PACKAGE OVERLAY - T31120'             
T31120   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**INFP**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         SPACE 2                                                                
* PACKAGE DISPLAY                                                               
*                                                                               
PAK      LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         TM    MODE,DISPLAY        TEST FOR CHANGE IN BASE FIELDS               
         BZ    *+8                                                              
         OI    MODE,FIRST          YES-RE-START READ                            
         TM    SVLMODE,EOF         TEST FOR EOF ON LAST DISPLAY                 
         BZ    *+8                                                              
         OI    MODE,FIRST          FORCE FIRST TIME                             
*                                                                               
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,X'FF'         TEST IF COMMA FOUND                          
         BE    PACK12              NO                                           
         MVI   FNDX,1                                                           
         SPACE                                                                  
* EDIT ACTION FIELD FOR FILTERS                                                 
*                                                                               
PACK2    XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'=,'                                                  
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BNE   PACK3                                                            
         CLI   FSTOP,X'FF'         TEST FOR END OF FIELD                        
         BE    PACK12              YES                                          
         B     PACKR               LONE TERMINATOR                              
         SPACE                                                                  
PACK3    ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         CLI   FSTOP,EQUAL         TEST FOR EQUALS SIGN                         
         BNE   PACK5               NO                                           
         CLI   FLDH+5,2                                                         
         BNE   PACKR                                                            
         CLC   FLD(2),=C'DP'       TEST FOR DAYPART                             
         BNE   PACKR                                                            
         XC    FTERM,FTERM         LOOK FOR DAYPART VALUE                       
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,2                                                         
         BH    PACKR                                                            
         CLI   DAYPART,0           TEST FOR DUPLICATE VALUE                     
         BNE   PACKR                                                            
         MVC   HALF,FLD                                                         
         OI    HALF+1,X'40'                                                     
         GOTO1 VALDAYPT,DMCB,(0,HALF)                                           
         CLC   KEY,KEYSAVE                                                      
         BNE   *+14                                                             
         MVC   DAYPART,KEY+5        DAYPART EQUATE NUMBER                       
         B     PACK10                                                           
*                                                                               
         MVC   XTRA(16),=C'BAD DAYPART CODE'                                    
         B     PACKR                                                            
         SPACE                                                                  
PACK5    ZIC   R1,FLDH+5           VALIDATE PACKAGE STATUS                      
         BCTR  R1,0                                                             
         LA    R0,PAKENT                                                        
         LA    RE,PAKTAB                                                        
*                                                                               
PACK6    CLC   FLDH+5(1),8(RE)     TEST FOR MINIMUM INPUT LENGTH                
         BL    PACK7               NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    PACK8                                                            
PACK7    LA    RE,L'PAKTAB(RE)                                                  
         BCT   R0,PACK6                                                         
         B     PACKR                                                            
         SPACE                                                                  
PACK8    ICM   R1,7,9(RE)          DISPLACEMENT TO FIELD FOR VALUE              
         LA    R1,TEMPD(R1)                                                     
         CLI   0(R1),0                                                          
         BNE   PACKR                                                            
         MVC   0(1,R1),FLD         SET PARM VALUE                               
         B     PACK10                                                           
         SPACE                                                                  
PACKR    MVI   FERN,INVERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
PACK10   B     PACK2               CONTINUE EDIT                                
         SPACE                                                                  
* TEST IF ANY READ CONTROLS HAVE CHANGED                                        
*                                                                               
PACK12   CLC   THISVALS,SVVALS     TEST FOR CHANGE IN CONTROL VALUES            
         BE    *+8                                                              
         OI    MODE,FIRST          YES                                          
         MVC   SVVALS(SVVALLN),THISVALS UPDATE CONTROL VALUES                   
         TM    MODE,FIRST          TEST FOR FIRST TIME READ                     
         BZ    PACK14                                                           
*                                                                               
         BAS   RE,CLRBLOCK         CLEAR SAVED BLOCK                            
         MVC   NBSELEST(2),EST     SET ESTIMATE VALUES                          
         TM    NETTYP,ISINGLE      TEST IF ONE NETWORK INPUT                    
         BZ    *+10                NO                                           
         MVC   NBSELNET,NET        YES-SET IT                                   
         MVC   NBSELDP,DAYPART                                                  
         MVC   NBSELPST,LOCK                                                    
         CLI   LOCK,0              TEST FOR LOCKED STATUS INPUT                 
         BNE   *+8                 YES                                          
         MVI   NBSELPST,C'B'       NO-DEFAULT IS TO RETURN BOTH                 
         CLI   LOCK,C'U'           TEST FOR UNLOCKED                            
         BNE   *+8                                                              
         MVI   NBSELPST,0          YES-MAKE USE OF NETIO DEFAULT                
         MVI   NBSELMOD,NBPROCPK                                                
         MVI   NBDATA,C'P'                                                      
         B     PACK15                                                           
         SPACE                                                                  
PACK14   BAS   RE,RESTBLK          RESTORE SAVED BLOCK                          
         MVI   NBFUNCT,NBFRDHI                                                  
         B     PACK15                                                           
         SPACE 2                                                                
PACK15   MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         LA    R2,INPDAT1H         POINT TO FIRST LINE                          
         GOTO1 VCLEARF,DMCB,(1,(R2)),INPLAST                                    
         LA    R3,LINES            COUNTER                                      
         SPACE                                                                  
* READ RECORDS AFTER SAVING BLOCK AND THEN APPLY FILTERS                        
*                                                                               
PACK16   BAS   RE,SAVBLOCK                                                      
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    PACK17                                                           
         CLI   NBERROR,NBINVEST    INTERCEPT NO ESTIMATES                       
         BE    NOESTERR                                                         
         CLI   NBERROR,NBINVPRD    INTERCEPT NO PRODUCTS                        
         BE    NOPRDERR                                                         
         DC    H'0'                                                             
NOESTERR MVC   XTRA(18),=C'NO ESTIMATE EXISTS'                                  
         B     PACKR                                                            
NOPRDERR MVC   XTRA(17),=C'NO PRODUCT EXISTS'                                   
         B     PACKR                                                            
*                                                                               
*        MVI   NBMODE,NBREQLST     FORCE END-OF-FILE                            
         SPACE 1                                                                
PACK17   CLI   NBMODE,NBREQLST     TEST FOR END-OF-READ                         
         BE    PACK20                                                           
         CLI   NBMODE,NBPROCPK                                                  
         BNE   PACK16                                                           
         CLI   FROZEN,0            TEST FOR PACKAGE FROZEN FILTER               
         BE    PACK18              NO                                           
         L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         CLI   FROZEN,C'F'                                                      
         BNE   *+16                                                             
         TM    NPAKSTAT,X'80'                                                   
         BO    PACK18                                                           
         B     PACK16                                                           
         TM    NPAKSTAT,X'80'      MUST BE UNFROZEN                             
         BO    PACK16              YES-SKIP IT                                  
*                                                                               
PACK18   CLI   AUDITSW,0           TEST FOR PACKAGE FROZEN FILTER               
         BE    PACK18B             NO                                           
         L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         TM    NPAKSTAT,X'02'                                                   
         BO    PACK18B                                                          
         B     PACK16                                                           
*                                                                               
PACK18B  CLI   UPLSET,0            TEST PACKAGE FOR UPLSET STATUS               
         BE    PACK19              NO                                           
         L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         TM    NPAKCNTL,X'08'                                                   
         BO    PACK19                                                           
         B     PACK16                                                           
*                                                                               
         SPACE                                                                  
* FORMAT A LINE OF DISPLAY DATA                                                 
*                                                                               
         USING PAKLIND,R2                                                       
PACK19   MVC   PAKNET,NPKNET                                                    
         ZIC   R0,NPKEST                                                        
         EDIT  (R0),(3,PAKEST)                                                  
         ZIC   R0,NPKPACK                                                       
         EDIT  (R0),(3,PAKPACK)                                                 
         MVC   PAKNAME,NBPAKNAM                                                 
*  DAYPART                                                                      
         LA    RE,PAKDPN                                                        
         MVC   0(2,RE),NBACTNDP                                                 
         LA    RE,1(RE)                                                         
         CLI   0(RE),X'40'                                                      
         BNH   *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         MVC   0(8,RE),NBDPNAM                                                  
         MVC   8(6,RE),NBDPNAM2                                                 
*                                                                               
         L     R0,NBPAKCST                                                      
         EDIT  (R0),(10,PAKCOST),COMMAS=YES,FLOAT=$,ALIGN=LEFT                  
         MVC   PAKSTAT(4),=C'OPEN'                                              
         TM    NPAKSTAT,X'80'                                                   
         BZ    *+14                                                             
         MVC   PAKSTAT(6),=C'FROZEN'                                            
         B     *+18                                                             
         TM    NPAKSTAT,X'20'                                                   
         BZ    *+10                                                             
         MVC   PAKSTAT(6),=C'LOCKED'                                            
         TM    NPAKCNTL,X'08'                                                   
         BZ    *+10                                                             
         MVC   PAKSTAT(6),=C'UPLSET'                                            
         MVI   RECSW,YES                                                        
         LA    R2,LINELEN(R2)                                                   
         BCT   R3,PACK16                                                        
         BAS   RE,SAVBLOCK         UPDATE SAVED BLOCK FOR LAST RECORD           
         SPACE                                                                  
* END OF FILE OR SCREEN PROCESSING                                              
*                                                                               
PACK20   MVI   MORESW,YES                                                       
         CLI   NBMODE,NBPROCPK     TEST IF PACKAGE RETURNED                     
         BE    PACK25              YES-HAVE MORE RECORDS TO READ                
         MVI   MORESW,NO           HAVE REACHED EOF                             
         CLI   RECSW,YES           TEST FOR ANY RECORDS TO DISPLAY              
         BNE   PACK26              NO-NO RECORDS QUALIFIED FOR DISPLAY          
         SPACE                                                                  
PACK25   MVC   BUYMSG(L'INFOMSG),INFOMSG                                        
         CLI   MORESW,YES          TEST FOR MORE TO COME                        
         BNE   *+14                                                             
         MVC   BUYMSG+L'INFOMSG+1(14),=C'(MORE TO COME)'                        
         B     *+10                                                             
         MVC   BUYMSG+L'INFOMSG+1(20),=C'- ENTER NEXT REQUEST'                  
         CLI   NBMODE,NBREQLST     TEST FOR END OF READ                         
         BE    PACK28              YES                                          
         B     PACK30                                                           
         SPACE 1                                                                
PACK26   MVC   BUYMSG(L'NODATA),NODATA                                          
         SPACE                                                                  
PACK28   LA    RE,SVDATA           CLEAR SAVE AREA TO FORCE RE-START            
         LA    RF,SVDATAL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   SVLMODE,EOF         FORCE FIRST TIME NEXT                        
         B     PACKX                                                            
         SPACE                                                                  
PACK30   MVI   SVLMODE,PROCESS     PROCESSED THIS TIME                          
         SPACE                                                                  
PACKX    LA    R2,BUYACTH                                                       
         OI    6(R2),X'01'         MODIFIED NEXT TIME                           
         ST    R2,FADDR                                                         
         NI    MODE,X'FF'-FIRST-DISPLAY                                         
         B     EXXMOD                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR SAVED BLOCK                                              
*                                                                               
CLRBLOCK ST    RE,SAVEREG          SAVE RETURN POINT                            
         LA    RE,SVNBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO RESTORE SAVED BLOCK                                            
*                                                                               
RESTBLK  ST    RE,SAVEREG                                                       
         LA    RE,NEBLOCKA                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,SVNBLOCK                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE NETBLOCK IN TWA SAVE AREA                                 
*                                                                               
SAVBLOCK ST    RE,SAVEREG                                                       
         LA    RE,SVNBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,NEBLOCKA                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
INFOMSG  DC    C'** PACKAGE(S) DISPLAYED'                                       
NODATA   DC    C'** NO DATA TO DISPLAY - ENTER NEXT REQUEST **'                 
         SPACE 2                                                                
* TABLE OF PACKAGE STATUS FILTERS                                               
*                                                                               
* BYTES 0-7 = CHARACTER STATUS FILTER                                           
* BYTE  8   = MININUM NUMBER OF CHARACTERS                                      
* BYTES 9-11= DISPLACEMENT INTO LOCAL STORAGE FOR FILTER VALUE                  
*                                                                               
PAKTAB   DS    0CL12                                                            
         DC    CL8'LOCKED',X'01',AL3(LOCK-TEMPD)                                
         DC    CL8'UNLOCKED',X'03',AL3(LOCK-TEMPD)                              
         DC    CL8'FROZEN',X'02',AL3(FROZEN-TEMPD)                              
         DC    CL8'UNFROZEN',X'03',AL3(FROZEN-TEMPD)                            
         DC    CL8'AUDITSW',X'02',AL3(AUDITSW-TEMPD)                            
         DC    CL8'UPLSET',X'02',AL3(UPLSET-TEMPD)                              
PAKENT   EQU   (*-PAKTAB)/L'PAKTAB                                              
         SPACE 2                                                                
* TABLE OF DAYPART CODES                                                        
*                                                                               
DPTTAB   DC    C'DFPKTYSENLCX'                                                  
DAYPARTS EQU   *-DPTTAB                                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* INFO SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF8D                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    0D                                                               
SVLMODE  DS    X                   LAST TIME PROCESS MODE                       
*                                                                               
SVVALS   DS    0C                  SAVED CONTROL VALUES                         
SVDP     DS    C                                                                
SVLOCK   DS    C                                                                
SVFROZEN DS    C                                                                
SVVALLN  EQU   *-SVVALS            LENGTH OF CONTROL VALUES                     
*                                                                               
SVNBLOCK DS    XL(NEBLOCKL)                                                     
SVDATAL  EQU   *-SVDATA                                                         
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
MORESW   DS    C                                                                
*                                  INFO PACKAGE DETAILS                         
THISVALS DS    0CL(SVVALLN)        THIS TIME CONTROL VALUES                     
DAYPART  DS    C                                                                
LOCK     DS    C                                                                
FROZEN   DS    C                                                                
AUDITSW  DS    C                                                                
UPLSET   DS    C                                                                
*                                                                               
RECSW    DS    C                                                                
*                                                                               
         DS    0D                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
* DSECT TO COVER SCREEN LINE ON PACKAGE SCREEN                                  
*                                                                               
PAKLIND  DSECT                                                                  
PAKFLDH  DS    CL8                 FIELD HEADER                                 
         DS    C                   SPARE                                        
PAKNET   DS    CL4                                                              
         DS    C                                                                
PAKEST   DS    CL3                                                              
         DS    CL2                                                              
PAKPACK  DS    CL3                                                              
         DS    CL2                                                              
PAKNAME  DS    CL16                                                             
         DS    C                                                                
PAKDPN   DS    CL17                                                             
         DS    C                                                                
PAKCOST  DS    CL10                                                             
         DS    C                                                                
PAKSTAT  DS    CL6                                                              
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
SLASH    EQU   C'/'                                                             
EQUAL    EQU   C'='                                                             
LINELEN  EQU   INPDAT2H-INPDAT1H                                                
LINES    EQU   (INPLAST-INPDAT1H)/LINELEN                                       
PROCESS  EQU   X'01'               PROCESSED DISPLAY LAST TIME                  
EOF      EQU   X'02'               FINISHED DISPLAY LAST TIME                   
         SPACE 2                                                                
* DDCOMFACS                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEBUY20   12/13/10'                                      
         END                                                                    
