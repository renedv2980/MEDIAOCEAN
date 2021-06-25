*          DATA SET DDPANDIMAN AT LEVEL 021 AS OF 05/01/02                      
*PHASE PANDIMAA PANDIMAN                                                        
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'PANDIMAN - DDS SOURCE LIBRARY LISTING'                          
PANDIMAN CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 500,**PMAN**,=V(REGSAVE),R9                                      
         USING PAND,RC                                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE+21(18),=C'DDS SOURCE LIBRARY'                              
         MVI   MID1,0                                                           
         MVC   SUB1(120),TITA                                                   
         MVC   SUB2(120),TITB                                                   
         OPEN  KHIN                                                             
         L     R2,=A(SORTBLOC)                                                  
         GOTO1 =V(SORTER),PARA,SORTCARD,RECCARD,(40,(R2))                       
         SPACE 1                                                                
         L     R8,=V(BOXAREA)      SET UP BOXES                                 
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         LA    R3,BOXCOLS                                                       
         MVI   000(R3),C'L'                                                     
         MVI   019(R3),C'C'                                                     
         MVI   030(R3),C'C'                                                     
         MVI   045(R3),C'C'                                                     
         MVI   052(R3),C'C'                                                     
         MVI   065(R3),C'C'                                                     
         MVI   073(R3),C'C'                                                     
         MVI   081(R3),C'C'                                                     
         MVI   090(R3),C'C'                                                     
         MVI   105(R3),C'C'                                                     
         MVI   118(R3),C'R'                                                     
         SPACE 1                                                                
         LA    R3,BOXROWS                                                       
         MVI   4(R3),C'T'                                                       
         MVI   7(R3),C'M'                                                       
         MVI   58(R3),C'B'                                                      
         EJECT                                                                  
*              INTERPRET CONTROL CARDS                                          
         SPACE 3                                                                
CONC     GOTO1 =V(CARDS),PARA,=C'RE00',C                                        
         CLC   C(2),=C'/*'                                                      
         BE    CONEND                                                           
         CLC   C(7),=C'SYSTEM='                                                 
         BNE   CON2                                                             
         LA    R2,C+7                                                           
         LA    R3,SYSMASK                                                       
         BAS   RE,MASKSET                                                       
         B     CONC                                                             
         SPACE 1                                                                
CON2     CLC   C(5),=C'TYPE='                                                   
         BNE   CON4                                                             
         LA    R2,C+5                                                           
         LA    R3,TYPEMASK                                                      
         BAS   RE,MASKSET                                                       
         B     CONC                                                             
         SPACE 1                                                                
CON4     DS    0H                                                               
         SPACE 1                                                                
CONEND   DS    0H                                                               
         EJECT                                                                  
*              READ DIRECTORY ITEMS                                             
         SPACE 3                                                                
DIRECT   GET   KHIN,D                                                           
         XC    SORTKEY,SORTKEY                                                  
         MVC   SORTUSER,D+13                                                    
         MVC   SORTBOOK,D                                                       
         MVC   SORTLEV,D+10                                                     
         MVC   SORTLANG,D+18                                                    
         MVC   SORTSTAT,D+23                                                    
         MVC   SORTMNT(3),D+56                                                  
         MVI   SORTMNT+3,C' '                                                   
         MVC   SORTMNT+4(8),D+26   MM/DD/YY                                     
         PACK  DUB,D+26(2)                                                      
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   SORTMNT+4(3),0(R1)                                               
         PACK  SORTSIZE,D+47(8)                                                 
         BAS   RE,FILTER                                                        
         BNE   DIRECT                                                           
         GOTO1 =V(SORTER),PARA,=C'PUT',SORTREC                                  
         B     DIRECT                                                           
         SPACE 1                                                                
DIREOF   DS    0H                                                               
         EJECT                                                                  
*              HANDLE SORT RECORDS AND REPORT                                   
         SPACE 3                                                                
GET      GOTO1 =V(SORTER),PARA,=C'GET'                                          
         L     R2,PARA+4                                                        
         LTR   R2,R2                                                            
         BNZ   GET2                                                             
         GOTO1 =V(PRINTER)                                                      
         OC    SORTKEY,SORTKEY                                                  
         BNZ   *+8                                                              
         BAS   RE,OWNTOTS                                                       
         BAS   RE,ENDTOTS                                                       
         GOTO1 =V(PRINT),PARA,=C'CLOSE'                                         
         CLOSE KHIN                                                             
         XBASE                                                                  
         SPACE 1                                                                
GET2     MVC   SORTREC,0(R2)                                                    
         OC    SORTKEY,SORTKEY                                                  
         BNZ   GET4                                                             
         CLC   LASTUSER,SPACES                                                  
         BE    GET4                                                             
         CLC   LASTUSER,SORTUSER                                                
         BE    GET4                                                             
         GOTO1 =V(PRINTER)         SPACE BETWEEN USER CODES                     
         CLC   LASTUSER(2),SORTUSER                                             
         BE    GET4                                                             
         BAS   RE,OWNTOTS                                                       
         ZAP   LINE,=P'75'         SKIP AFTER USER TOTS                         
         SPACE 1                                                                
GET4     MVC   LASTUSER,SORTUSER                                                
         BAS   RE,FORMAT                                                        
         B     GET                                                              
         EJECT                                                                  
*              ROUTINE TO SET MASK AND FILTER                                   
         SPACE 3                                                                
MASKSET  NTR1                                                                   
         MVC   0(9,R3),=9C'N'                                                   
         SPACE 1                                                                
SM2      CLI   0(R2),X'F0'                                                      
         BL    XIT                                                              
         PACK  DUB,0(1,R2)                                                      
         CVB   R1,DUB                                                           
         AR    R1,R3                                                            
         MVI   0(R1),C'Y'                                                       
         B     SM2                                                              
         SPACE 2                                                                
FILTER   PACK  DUB,SORTUSER+2(1)   SYSTEM 0-9                                   
         CVB   R1,DUB                                                           
         LA    R1,SYSMASK(R1)                                                   
         CLI   0(R1),C'Y'                                                       
         BNER  RE                                                               
         SPACE 1                                                                
         PACK  DUB,SORTUSER+3(1)   TYPE 0-9                                     
         CVB   R1,DUB                                                           
         LA    R1,TYPEMASK(R1)                                                  
         CLI   0(R1),C'Y'                                                       
         BR    RE                                                               
         EJECT                                                                  
*              FORMAT A LINE OF PRINT                                           
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         LA    R3,P                                                             
         LA    R2,SORTUSER         OWNER                                        
         LA    R4,2(R3)                                                         
         LA    R5,OWNTAB                                                        
         BAS   RE,TABOUT                                                        
         SPACE 1                                                                
         MVI   WORK,C'0'           SYSTEM                                       
         MVC   WORK+1(1),SORTUSER+2                                             
         LA    R2,WORK                                                          
         LA    R4,21(R3)                                                        
         LA    R5,SYSTAB                                                        
         BAS   RE,TABOUT                                                        
         SPACE 1                                                                
         MVC   WORK+1(1),SORTUSER+3                                             
         LA    R4,32(R3)           TYPE                                         
         LA    R5,TYPETAB                                                       
         CLI   SORTUSER,C'5'       SPECIAL TYPES FOR CS                         
         BNE   *+8                                                              
         LA    R5,CSTYPES                                                       
         CLI   SORTUSER,C'6'       AND FOR DC                                   
         BNE   *+8                                                              
         LA    R5,DCTYPES                                                       
         BAS   RE,TABOUT                                                        
         MVC   47(4,R3),SORTUSER                                                
         MVC   54(10,R3),SORTBOOK                                               
         MVC   68(3,R3),SORTLEV                                                 
         MVC   75(5,R3),SORTLANG                                                
         MVC   83(5,R3),=C'P/A/E'                                               
         MVC   83(1,R3),SORTSTAT                                                
         MVC   85(1,R3),SORTSTAT+1                                              
         MVC   87(1,R3),SORTSTAT+2                                              
         MVC   92(12,R3),SORTMNT                                                
         EDIT  (P4,SORTSIZE),(5,109(R3))                                        
         GOTO1 =V(PRINTER)                                                      
         AP    OWNACCS(8),=PL1'1'                                               
         AP    OWNACCS+8(8),SORTSIZE                                            
         AP    ALLACCS(8),=PL1'1'                                               
         AP    ALLACCS+8(8),SORTSIZE(4)                                         
         B     XIT                                                              
         EJECT                                                                  
*              TOTALLING ROUTINES AND TABLE LOOK UP                             
         SPACE 3                                                                
OWNTOTS  NTR1                                                                   
         LA    R2,OWNACCS                                                       
         B     ALLTOTS                                                          
         SPACE 1                                                                
ENDTOTS  NTR1                                                                   
         LA    R2,ALLACCS                                                       
         SPACE 1                                                                
ALLTOTS  LA    R3,P                                                             
         EDIT  (P8,0(R2)),(6,31(R3))                                            
         MVC   38(5,R3),=C'BOOKS'                                               
         EDIT  (P8,8(R2)),(8,106(R3))                                           
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         ZAP   0(8,R2),=P'0'                                                    
         ZAP   8(8,R2),=P'0'                                                    
         B     XIT                                                              
         SPACE 1                                                                
TABOUT   NTR1                                                                   
         SPACE 1                                                                
TABOUT1  CLC   0(2,R2),0(R5)       EXPAND TABLE                                 
         BE    TABOUT2                                                          
         CLI   0(R5),X'FF'                                                      
         BE    TABOUT2                                                          
         LA    R5,18(R5)                                                        
         B     TABOUT1                                                          
         SPACE 1                                                                
TABOUT2  MVC   0(16,R4),2(R5)                                                   
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
TITA     DC    C'  OWNER/PROGRAMMER   SYSTEM     TYPE OF '                      
         DC    C'BOOK   USER   BOOK NAME    LEVEL   LANG.'                      
         DC    C'   STATUS      LAST        NUMBER OF    '                      
         SPACE 1                                                                
TITB     DC    C'                                        '                      
         DC    C'       CODE                             '                      
         DC    C'            MAINTENANCE    STATEMENTS   '                      
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(3,31,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(48,,48)        '                      
         SPACE 1                                                                
SYSMASK  DC    10C'Y'                                                           
TYPEMASK DC    10C'Y'                                                           
LASTUSER DC    CL4' '                                                           
OWNACCS  DC    2PL8'0'                                                          
ALLACCS  DC    2PL8'0'                                                          
         DS    D                                                                
D        DS    CL88                                                             
         SPACE 1                                                                
KHIN     DCB   RECFM=FB,DDNAME=DISKIN,DSORG=PS,                        X        
               EODAD=DIREOF,MACRF=GM,LRECL=80                                   
         SPACE 1                                                                
OWNTAB   DS    0H                                                               
         DC    C'00UNCLAIMED       '                                            
         DC    C'10SYSTEMS US      '                                            
         DC    C'12KEN HAMILTON    '                                            
         DC    C'14MEL HERTZIG     '                                            
         DC    C'15GRANT PLATT     '                                            
         DC    C'16JOHN DILLON     '                                            
         DC    C'17BOB ZEHNDER     '                                            
         DC    C'18BRUCE PLATT     '                                            
         DC    C'19BOB LERNER      '                                            
         DC    C'20BOB YUKNAVECH   '                                            
         DC    C'21COLIN MORRIS    '                                            
         DC    C'22VICTOR PEI      '                                            
         DC    C'23PETER ZIRNIS    '                                            
         DC    C'24BOB GRIFFIN     '                                            
         DC    C'25EVAN SCHAPIRO   '                                            
         DC    C'26DAVID HABER     '                                            
         DC    C'30SYSTEMS UK      '                                            
         DC    C'31BOB CRIGHTON    '                                            
         DC    C'32JOHN NEWNHAM    '                                            
         DC    C'33TIM GUTCH       '                                            
         DC    C'34DICK TURNER     '                                            
         DC    C'35SIMON           '                                            
         DC    C'36TERRY           '                                            
         DC    C'50CLIENT SERVICE  '                                            
         DC    C'51CS ADMIN.       '                                            
         DC    C'54CS SERV. NOTICE '                                            
         DC    C'55CS PUBLICATION  '                                            
         DC    C'56CS PUB UPDATE   '                                            
         DC    C'58CS MISCELLANEOUS'                                            
         DC    C'59CS ASSEMBLER    '                                            
         DC    C'60DATA CONTROL    '                                            
         DC    C'70MARKET RESEARCH '                                            
         DC    C'80OPERATIONS      '                                            
         DC    C'90OTHERS          '                                            
         DC    X'FFFF'                                                          
         DC    CL16'UNKNOWN'                                                    
         SPACE 1                                                                
SYSTAB   DC    C'00FACILS          '                                            
         DC    C'01SPOTPAK         '                                            
         DC    C'02PRINTPAK        '                                            
         DC    C'03ACCPAK          '                                            
         DC    C'04CONTROL         '                                            
         DC    C'05PLANNING        '                                            
         DC    C'06REPPAK          '                                            
         DC    C'07NETPAK          '                                            
         DC    C'08OTHERS          '                                            
         DC    C'09NON-SYS         '                                            
         DC    X'FFFF'                                                          
         DC    CL16'UNKNOWN'                                                    
         SPACE 1                                                                
TYPETAB  DS    0H                                                               
         DC    C'00DOCUMENTATION   '                                            
         DC    C'01DSECT           '                                            
         DC    C'02SCREEN          '                                            
         DC    C'03MACRO           '                                            
         DC    C'04RELO (DOS)      '                                            
         DC    C'05RELO (MVS)      '                                            
         DC    C'06PHASE (DOS)     '                                            
         DC    C'07PHASE (MVS)     '                                            
         DC    C'08LINKEDT DECK    '                                            
         DC    C'09OTHERS          '                                            
         DC    X'FFFF'                                                          
         DC    CL16'UNKNOWN'                                                    
         SPACE 1                                                                
CSTYPES  DS    0H                                                               
         DC    C'00UNASSIGNED      '                                            
         DC    C'01CURRENT         '                                            
         DC    C'02DRAFT           '                                            
         DC    C'03HISTORICAL      '                                            
         DC    C'09OTHERS          '                                            
         DC    X'FFFF'                                                          
         DC    CL16'UNKNOWN'                                                    
         SPACE 1                                                                
DCTYPES  DS    0H                                                               
         DC    C'00UNASSIGNED      '                                            
         DC    C'01REQUESTS        '                                            
         DC    C'02DATA            '                                            
         DC    C'03NETGEN          '                                            
         DC    C'09OTHERS          '                                            
         DC    X'FFFF'                                                          
         DC    CL16'UNKNOWN'                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR MODULE                                                 
         SPACE 3                                                                
PAND     DSECT                                                                  
SORTREC  DS    0CL48                                                            
SORTKEY  DS    CL7                                                              
SORTUSER DS    CL4                                                              
SORTBOOK DS    CL10                                                             
SORTLEV  DS    CL3                                                              
SORTLANG DS    CL5                                                              
SORTSTAT DS    CL3                                                              
SORTMNT  DS    CL12                                                             
SORTSIZE DS    PL4                                                              
         SPACE 1                                                                
DUB      DS    D                                                                
WORK     DS    CL64                                                             
C        DS    CL80                                                             
PARA     DS    6F                                                               
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDBIGBOX                                                       
SORTBLOC CSECT                                                                  
         DC    41000X'00'                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DDPANDIMAN05/01/02'                                      
         END                                                                    
