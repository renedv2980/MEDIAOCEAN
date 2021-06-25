*          DATA SET SPLDEXTSDE AT LEVEL 104 AS OF 02/03/01                      
*PHASE SPEXTSDE                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - FIX WILA STATION BUCKET RECORDS'                      
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
         USING STABUCKD,R3                                                      
         CLC   =X'0E01',STABKCOD   STATION BUCKET RECORD?                       
         BNE   DMXKEEP             NO, IGNORE IT                                
         TM    STABCNTL,X'80'      RECORD MARKED FOR DELETION?                  
         BO    DMXKEEP             YES, SKIP IT                                 
*                                                                               
         AP    NUMSTABS,=P'1'                                                   
*                                                                               
         LA    R2,BADKEYS                                                       
CHECKKEY CLI   0(R2),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   0(5,R2),STABKAM     COMPARE AGY/MED/CLT/PRD/EST                  
         BE    *+12                                                             
         LA    R2,L'BADKEYS(,R2)                                                
         B     CHECKKEY                                                         
*                                                                               
         MVC   P(6),=C'KEY = '                                                  
         MVC   P+6(13),5(R2)       PRINT EBCDIC KEY                             
*                                                                               
         MVI   DUP,C'N'                                                         
         CLC   OLDKEY(12),STABUCK                                               
         BNE   *+14                                                             
         MVC   P+25(19),=C'** DUPLICATE KEY **'                                 
         MVI   DUP,C'Y'                                                         
*                                                                               
*********CLC   OLDKEY(7),STABUCK                                                
*********BE    *+10                                                             
*********MVC   P+50(35),=C'** CHANGE OF AGY/MED/CLT/PRD/EST **'                 
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   DUP,C'Y'                                                         
         BNE   SAVEKEY                                                          
         CLC   STABLEN,=X'002B'    THESE SHOULD ALL BE SHORT RECORDS            
         BE    *+6                                                              
         DC    H'0'                MUST CHECK THIS ONE OUT                      
         CLC   STABBDT,=X'CA3D'    THEY RAN BILLING ON 1/29/01                  
         BE    SAVEKEY                                                          
         CLC   STABBDT,=X'CA3E'    AND AGAIN ON 1/30/01                         
         BE    SAVEKEY                                                          
         CLC   STABBDT,=X'CA3F'    AND AGAIN ON 1/31/01                         
         BE    SAVEKEY                                                          
         DC    H'0'                MUST CHECK THIS ONE OUT                      
*                                                                               
SAVEKEY  MVC   OLDKEY,STABUCK                                                   
*                                                                               
         CLI   STABKCUR,0          BETTER START AS NON-COST2                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   STABKCUR,1          TURN ON THE COST2 BIT IN THE KEY             
*                                                                               
         AP    NUMCHNGD,=P'1'                                                   
         GOTO1 =V(PRTREC),DMCB,0(R3),(24,13),V(PRINT),V(HEXOUT)                 
         MVI   P,0                                                              
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R3                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(40),=C'NUMBER OF STATION BUCKET RECORDS SEEN = '               
         EDIT  NUMSTABS,(15,P+40),ALIGN=LEFT,COMMAS=YES                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(26),=C'NUMBER OF RECORDS FIXED = '                             
         EDIT  NUMCHNGD,(15,P+26),ALIGN=LEFT,COMMAS=YES                         
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0024'                                                          
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
BADKEYS  DS    0XL18                                                            
         DC    X'1282380102',C'R ARY CH  002'                                   
         DC    X'1282CA010B',C'R AWK CH  011'                                   
         DC    X'12840F0E02',C'R BAP GLA 002'                                   
         DC    X'12840F0EEE',C'R BAP GLA 238'                                   
         DC    X'12840F18EA',C'R BAP SCA 234'                                   
         DC    X'12844E1A07',C'R BCO TGO 007'                                   
         DC    X'1284521256',C'R BCS NPR 086'                                   
         DC    X'1286450104',C'R BSF RES 004'                                   
         DC    X'128655020E',C'R BSV BNU 014'                                   
         DC    X'1288D60505',C'R CGW CEN 005'                                   
         DC    X'1288D60301',C'R CGW GUL 001'                                   
         DC    X'1288D60302',C'R CGW GUL 002'                                   
         DC    X'1288D60403',C'R CGW SE  003'                                   
         DC    X'1288D60404',C'R CGW SE  004'                                   
         DC    X'12946B0101',C'R FDL SU  001'                                   
         DC    X'12946B010E',C'R FDL SU  014'                                   
         DC    X'129B721AB3',C'R G2S SW  179'                                   
         DC    X'129B92033C',C'R G3S BS  060'                                   
         DC    X'129F43202A',C'R H1D PS3 042'                                   
         DC    X'12A26D01CC',C'R ITN TS  204'                                   
         DC    X'12AF3B0AC5',C'R LZ2 EL  197'                                   
         DC    X'12AF3B1AB3',C'R LZ2 SW  179'                                   
         DC    X'12AF3C184E',C'R LZ3 LA  078'                                   
         DC    X'12B1110140',C'R MIR HC  064'                                   
         DC    X'12C73B1BB3',C'R RZ2 SW  179'                                   
         DC    X'12C73C0341',C'R RZ3 BS  065'                                   
         DC    X'1188D6050F',C'T CGW CEN 015'                                   
         DC    X'1188D60523',C'T CGW CEN 035'                                   
         DC    X'1188D60529',C'T CGW CEN 041'                                   
         DC    X'1188D6030B',C'T CGW GUL 011'                                   
         DC    X'1188D6030C',C'T CGW GUL 012'                                   
         DC    X'1188D60315',C'T CGW GUL 021'                                   
         DC    X'1188D60316',C'T CGW GUL 022'                                   
         DC    X'1188D6031F',C'T CGW GUL 031'                                   
         DC    X'1188D60320',C'T CGW GUL 032'                                   
         DC    X'1188D6040D',C'T CGW SE  013'                                   
         DC    X'1188D6040E',C'T CGW SE  014'                                   
         DC    X'1188D60417',C'T CGW SE  023'                                   
         DC    X'1188D60418',C'T CGW SE  024'                                   
         DC    X'1188D60421',C'T CGW SE  033'                                   
         DC    X'1188D60422',C'T CGW SE  034'                                   
         DC    X'11946B0101',C'T FDL SU  001'                                   
         DC    X'11946B010B',C'T FDL SU  011'                                   
         DC    X'119B720AB4',C'T G2S EL  180'                                   
         DC    X'119B721A94',C'T G2S SW  148'                                   
         DC    X'119C7AB3E9',C'T HD1 ST6 233'                                   
*********DC    X'119F434C15',C'T H1D SS2 021'                                   
*********DC    X'119F434C16',C'T H1D SS2 022'                                   
         DC    X'11AF3B0AB4',C'T LZ2 EL  180'                                   
         DC    X'11AF3B1A94',C'T LZ2 SW  148'                                   
         DC    X'11B2510510',C'T MSR WSB 016'                                   
         DC    X'11C4FA01D4',C'T RH1 FR  212'                                   
         DC    X'11C73B0BB4',C'T RZ2 EL  180'                                   
         DC    X'11C73B1B94',C'T RZ2 SW  148'                                   
         DC    X'11C73C1208',C'T RZ3 FUR 008'                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*COUNTER'                                                      
NUMSTABS DC    PL8'0'                                                           
NUMCHNGD DC    PL8'0'                                                           
OLDKEY   DC    XL13'00'                                                         
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
DUP      DS    C                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENSTAB                                                                      
       ++INCLUDE SPGENSTAB                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104SPLDEXTSDE02/03/01'                                      
         END                                                                    
